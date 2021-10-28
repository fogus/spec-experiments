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

(defn- ensure-checking-fn
  "Builds a thunk and its lexical environment used to instrument a function
  and perform Spec checking on its arguments at runtime."
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

(defn- find-varargs-decl
  "Inspects a arglist to find a varargs declarator, i.e. the element after
  the ampersand, and returns it if found."
  [arglist]
  (let [[_ decl :as restargs] (->> arglist
                                   (split-with (complement #{'&}))
                                   second)]
    (and (= 2 (count restargs))
         decl)))

(defn- has-kwargs? [arglists]
  (->> arglists (some find-varargs-decl) map?))

(defn- unmappify [args]
  (if (even? (count args))
     args
     (concat (butlast args)
             (reduce-kv (fn [acc k v] (->> acc (cons v) (cons k)))
                        ()
                        (last args)))))

(defn- process-fixed-args
  "Takes an arglist and returns a vector of symbols pertaining to the
  fixed arguments in the original arglist."
  [arglist]
  (->> arglist (take-while (complement #{'&})) (map (fn [_] (gensym))) vec))

(defn- kwargs-body
  "Builds a function body pertaining to a keyword arguments arity.
  A varargs arity is built with a processing chain for the
  incoming arguments that detects if a trailing argument exists and
  attempts to convert it to a seq of key->val pairs. This seq is the
  input to the underlying function prefixed by any named arguments."
  [f arglist]
  (let [alias (gensym "kvs")
        head-args (process-fixed-args arglist)]
    (list (conj head-args '& alias)
          `(apply ~f ~@head-args (@#'unmappify ~alias)))))

(defn- varargs-body
  "Builds a function body pertaining to a varargs arity. Builds the
  call to the underlying function by supplying any named parameters
  and the varargs parameter to apply."
  [f arglist]
  (let [head-args (process-fixed-args arglist)
        alias  (gensym "args")]
    (list (conj head-args '& alias)
          `(apply ~f ~@head-args ~alias))))

(defn- fixed-args-body
  "Builds an argument context for fixed arities. The arguments
  are taken directly from the parameter list."
  [f arglist]
  (let [arglist (process-fixed-args arglist)]
    (list arglist
          `(~f ~@arglist))))

(defn- build-flattener-struct
  "Takes an arglists data-structure and generates data for eval
  defining a HOF that given a function returns a function of
  analgous arglists that passes arguments on to the closed over
  function."
  [arglists]
  (let [closed-over-name (gensym "inner")]
    `(fn [~closed-over-name]
       (fn ~@(map (fn [arglist]
                    (let [varargs-decl (find-varargs-decl arglist)]
                      (cond (map? varargs-decl) (kwargs-body     closed-over-name arglist)
                            varargs-decl        (varargs-body    closed-over-name arglist)
                            :default            (fixed-args-body closed-over-name arglist))))
                  (or arglists
                      '([& args])))))))

(comment
  ;; The flattener generated is below (with some gensym name cleanup for readability)
  (fn [inner]
    (fn
      ([G__a] (inner G__a))
      ([G__a G__b] (inner G__a G__b))
      ([G__a G__b & G__kvs]
       (apply inner G__a G__b (if (even? (count G__kvs))
                                kvs
                                (reduce-kv (fn [acc k v]
                                             (->> acc (cons v) (cons k)))
                                                     (butlast G__kvs)
                                                     (last G__kvs)))))))
)

(defn- maybe-wrap-kvs-emulation
  "Takes an argslist and function. If the arglists contain an arity
  for keyword-arguments then a HOF is generated that returns a
  wrapping function that flattens a trailing map into a key->val
  seq if present. The seq is then used as supplemental arguments
  to the function f."
  [f arglists]
  (if (has-kwargs? arglists)
    (let [flattener-struct (build-flattener-struct arglists)
          kvs-emu (eval flattener-struct)]
      (kvs-emu f))
    f))

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
            arglists (->> v meta :arglists (sort-by count) seq)
            checked (ensure-checking-fn v ofn ospec)
            wrapped (maybe-wrap-kvs-emulation checked arglists)]
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

