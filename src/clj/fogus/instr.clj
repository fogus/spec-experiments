(ns fogus.instr
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest :exclude (instrument)]))

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

(defn unstrument-local
  [s]
  (when-let [v (resolve s)]
    (when-let [{:keys [raw wrapped]} (get @instrumented-vars v)]
      (swap! instrumented-vars dissoc v)
      (let [current @v]
        (when (= wrapped current)
          (alter-var-root v (constantly raw))
          (->sym v))))))

(declare instrument-1)

(defn- collectionize
  [x]
  (if (symbol? x)
    (list x)
    x))

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

(defn- build-xform [local]
  `(if (even? (count ~local))
     ~local
     (concat (butlast ~local)
             (reduce (fn [acc# ^java.util.Map$Entry me#]
                       (conj acc# (.getKey me#) (.getValue me#)))
                     []
                     (last ~local)))))

(comment

  (defn xform [& local]
    (if (even? (count local))
      local
      (let [trail (last local)]
        (concat (butlast local)
                (reduce (fn [acc ^java.util.Map$Entry me]
                          (conj acc (.getKey me) (.getValue me)))
                        []
                        trail)))))

  (xform :a 1 :b 2)
  (xform :a 1 {:b 2})
  (xform :a 1 :b)
  (xform :a 1 (java.util.HashMap. {:b 2 :c 3}))
  (xform :a 1 {:b 2 :c 3})
  (xform [(java.util.AbstractMap$SimpleEntry. :a 1) (java.util.AbstractMap$SimpleEntry. :b 2)])
  (xform [(clojure.lang.MapEntry/create :a 1) (clojure.lang.MapEntry/create :b 2)])
)

(defn- kwargs-context
  "Builds an arguments context for the function body pertaining to a
  keyword arguments arity. Inspects the kwargs declarator (i.e. & {})
  and adds an :as declaration if missing. Also builds a processing
  chain for the :data that converts the incoming map named by :as into
  a seq of key->val pairs. This :data process is meant to serve as the
  input to an arg spec. Finally, rebuilds the arglist to have the
  ammended kwargs declarator."
  [arglist decl]
  (let [as-name 'kvs
        decl (assoc decl :as as-name)
        head-args (->> arglist (take-while (complement #{'&})) vec)]
    {:args    (conj head-args as-name)
     :data    `[~@head-args ~(build-xform as-name)]
     :arglist (vec (concat head-args ['& as-name]))
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
           (list (or (:arglist context arglist))
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

(defn instrument-local
  ([sym-or-syms opts]
     (locking instrumented-vars
       (into
        []
        (comp (filter (instrumentable-syms opts))
              (distinct)
              (map #(instrument-1 % opts))
              (remove nil?))
        (collectionize sym-or-syms)))))

(comment
  (instrument-1 `add10 {})
  (instrument-1 `kwargs-fn {})

  (clojure.core/fn [inner]
    (clojure.core/fn
      ([opts] (inner opts))
      ([a b] (inner a b))
      ([a b & kvs]
       (clojure.core/apply inner a b (if (clojure.core/even? (clojure.core/count kvs))
                                       kvs
                                       (clojure.core/concat
                                        (clojure.core/butlast kvs)
                                        (clojure.core/reduce
                                         (clojure.core/fn [acc__10108__auto__ me__10109__auto__]
                                           (clojure.core/conj acc__10108__auto__ (.getKey me__10109__auto__) (.getValue me__10109__auto__)))
                                         []
                                         (clojure.core/last kvs))))))))
  
  (instrument-local `kwargs-fn {})

  (kwargs-fn 1)
  (kwargs-fn 1 2)
  (kwargs-fn 1 2 :a 1)
  (kwargs-fn 1 2 :a 1 {:b 2})
  (kwargs-fn 1 :B)
  (kwargs-fn 1 2 :a 1 {:b :B})

  (unstrument-local `kwargs-fn)

  (defn proc [& args]
    (if (odd? (count args))
      (let [trail (last args)]
        (if (map? trail)
          (concat (butlast args) (-> trail seq flatten))
          args))
      args))

  (proc :a 1 :b 2)
  (proc :a 1 :b 2 {:c 3})
  (proc)
  
)
