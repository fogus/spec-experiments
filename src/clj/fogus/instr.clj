(ns fogus.instr
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]))

(ns fogus.t
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]))

(set! *warn-on-reflection* true)

;;; Pull in instrument stuff
(use 'clojure.spec.test.alpha)
(def instrument-choose-spec (deref #'clojure.spec.test.alpha/instrument-choose-spec))
(def instrument-choose-fn (deref #'clojure.spec.test.alpha/instrument-choose-fn))
(def no-fspec (deref #'clojure.spec.test.alpha/no-fspec))
(def stacktrace-relevant-to-instrument (deref #'clojure.spec.test.alpha/stacktrace-relevant-to-instrument))
(def ^:private ^:dynamic *instrument-enabled*
  "if false, instrumented fns call straight through"
  true)
(defn- spec-checking-fn
  [v f fn-spec]
  (let [fn-spec (@#'s/maybe-spec fn-spec)
        conform! (fn [v role spec data args]
                   (let [conformed (s/conform spec data)]
                     (if (= ::s/invalid conformed)
                       (let [caller (->> (.getStackTrace (Thread/currentThread))
                                         stacktrace-relevant-to-instrument
                                         first)
                             ed (merge (assoc (s/explain-data* spec [] [] [] data)
                                         ::s/fn (->sym v)
                                         ::s/args args
                                         ::s/failure :instrument)
                                       (when caller
                                         {::caller (dissoc caller :class :method)}))]
                         (throw (ex-info
                                 (str "Call to " v " did not conform to spec.")
                                 ed)))
                       conformed)))]
    (fn
     [& args]
     (if *instrument-enabled*
       (with-instrument-disabled
         (when (:args fn-spec) (conform! v :args (:args fn-spec) args args))
         (binding [*instrument-enabled* true]
           (.applyTo ^clojure.lang.IFn f args)))
       (.applyTo ^clojure.lang.IFn f args)))))


(defonce ^:private instrumented-vars (atom {}))

(defn- unstrument-1
  [s]
  (when-let [v (resolve s)]
    (when-let [{:keys [raw wrapped]} (get @instrumented-vars v)]
      (swap! instrumented-vars dissoc v)
      (let [current @v]
        (when (= wrapped current)
          (alter-var-root v (constantly raw))
          (->sym v))))))

;; test funs

(defn kwargs-fn
  ([opts] opts)
  ([a b] [a b])
  ([a b & {:as m}] [a b m]))

(defn no-kwargs-fn
  ([opts] opts)
  ([a b] [a b])
  ([a b & opts] [a b opts]))

(defn just-varargs [& args]
  (apply + args))

(defn add10 [n]
  (+ 10 n))

(alter-meta! #'add10 dissoc :arglists)

;;; Specs

(s/def ::a any?)
(s/def ::b number?)
(s/def ::c any?)

(s/fdef kwargs-fn
  :args (s/alt :unary  (s/cat :a ::a)
               :binary (s/cat :a ::a :b ::b)
               :variadic (s/cat :a ::a
                                :b ::b
                                :kwargs (s/keys* :opt-un [::a ::b ::c]))))

(s/fdef no-kwargs-fn
  :args (s/alt :unary  (s/cat :a ::a)
               :binary (s/cat :a ::a :b ::b)
               :variadic (s/cat :a ::a
                                :b ::b
                                :varargs (s/cat :numbers (s/* number?)))))

(s/fdef just-varargs
  :args (s/cat :numbers (s/* number?))
  :ret number?)

(s/fdef add10
  :args (s/cat :arg ::b)
  :ret number?)

;;; macro utils

(defn- varargs
  "Inspects a arglist to find a varargs declarator, i.e. the element after
  the ampersand and returns it if found."
  [arglist]
  (let [[_ decl :as restargs] (->> arglist
                                   (split-with (complement #{'&}))
                                   second)]
    (and (= 2 (count restargs))
         decl)))

(defn- kwargs-context
  "Builds an arguments context for the function body pertaining to a
  keyword arguments arity. Inspects the kwargs declarator (i.e. & {})
  and adds an :as declaration if missing. Also builds a processing
  chain for the :data that converts the incoming map named by :as into
  a seq of key->val pairs. This :data process is meant to serve as the
  input to an arg spec. Finally, rebuilds the arglist to have the
  ammended kwargs declarator."
  [arglist decl]
  (let [as-name (or (:as decl) (gensym "as"))
        decl (assoc decl :as as-name)
        head-args (->> arglist (take-while (complement #{'&})) vec)]
    {:args    (conj head-args as-name)
     :data    `[(->> ~as-name seq flatten (concat ~head-args))]
     :arglist (-> arglist butlast vec (conj decl))
     :decl    decl}))

(defn- varargs-context
  "Builds an arguments context for the function body pertaining to a
  varargs arity. Builds the :data and :args process supplied to args spec by
  concatenating any named parameters to the varargs parameter."
  [arglist decl]
  (if (map? decl)
    (kwargs-context arglist decl)
    (let [head-args (->> arglist (take-while (complement #{'&})) vec)
          args-sym  'args]
      {:arglist (vec (concat head-args '[& args]))
       :data    `(~@head-args ~args-sym)
       :args    `(~@head-args ~args-sym)
       :decl    decl})))

(defn- args-context
  "Builds an argument context for fixed arities. Both the :args and :data
  are taken from the parameter list as the intent is to use the eventual
  vector as an input to applyTo."
  [arglist]
  {:args    arglist
   :data    arglist
   :arglist arglist})

(defn- gen-body
  "Builds a spec thunk body from a given context. It's expected that the
  context contain the follwing mappings:

   :data the data process used to build the arguments to the arg spec
   :fun  the original function" 
  [context]
  `(~@(:call context) ~@(:data context)))

(defn- gen-bodies
  "Generates the function bodies corresponding to the arities found in the
  :arglists meta of the Var v. Takes an additional context map containing
  local names to capture in the resulting function for the original function
  under instrumentation and the Spec for that function."
  [v f]
  (map (fn [arglist]
         (let [context (if-let [decl (varargs arglist)]
                         (varargs-context arglist decl)
                         (args-context    arglist))]           
           (list arglist
                 (gen-body (merge context {:call (if (:decl context) [`apply f] [f])})))))
       (or (->> v meta :arglists (sort-by count) seq)
           '([& args]))))

(defn- gen-thunk
  "Builds a thunk and its lexical environment used to instrument a function
  and perform Spec checking at runtime."
  [v]
  (let [orig 'inner]
    `(fn [~orig]
       (fn
         ~@(gen-bodies v orig)))))

(defn- instrument-1
  [s opts]
  (when-let [v (resolve s)]
    (when-not (-> v meta :macro)
      (let [spec (s/get-spec v)
            {:keys [raw wrapped]} (get @instrumented-vars v)
            current @v
            to-wrap (if (= wrapped current) raw current)
            ospec (or (instrument-choose-spec spec s opts)
                      (throw (no-fspec v spec)))
            ofn (instrument-choose-fn to-wrap ospec s opts)
            checked (spec-checking-fn v ofn ospec)
            thunk (eval (gen-thunk v))
            wrapped (thunk checked)]
        (alter-var-root v (constantly wrapped))
        (swap! instrumented-vars assoc v {:raw to-wrap :wrapped wrapped})
        (->sym v)))))

(comment
  (instrument-1 `add10 {})
  (instrument-1 `kwargs-fn {})

  (clojure.core/fn [inner]
    (clojure.core/fn
      ([opts] (inner opts))
      ([a b] (inner a b))
      ([a b & {:as m}] (clojure.core/apply inner (clojure.core/->> m clojure.core/seq clojure.core/flatten (clojure.core/concat [a b]))))))
  
  (def f (instrument-1 `kwargs-fn {}))

  (f 1)
  (f 1 2)
  (f 1 2 :a 1)
  (f 1 2 :a 1 {:b 2})
  (f 1 :B)
  (f 1 2 :a 1 {:b :B})

  (instrument-1 `kwargs-fn {})

  (kwargs-fn 1)
  (kwargs-fn 1 2)
  (kwargs-fn 1 2 :a 1)
  (kwargs-fn 1 2 :a 1 {:b 2})
  (kwargs-fn 1 :B)
  (kwargs-fn 1 2 :a 1 {:b :B})

  (unstrument-1 `kwargs-fn)
)
