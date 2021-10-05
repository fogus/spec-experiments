(ns fogus.instr
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]))

(set! *warn-on-reflection* true)

(defn kwargs-fn
  ([opts] opts)
  ([a b] [a b])
  ([a b & {:as m}] [a b m]))

(defn no-kwargs-fn
  ([opts] opts)
  ([a b] [a b])
  ([a b & opts] [a b opts]))

(defn just-varargs [& args]
  (apply + )args)

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

;;; Prelim stuff

(use 'clojure.spec.test.alpha)
(def instrument-choose-spec (deref #'clojure.spec.test.alpha/instrument-choose-spec))
(def instrument-choose-fn (deref #'clojure.spec.test.alpha/instrument-choose-fn))
(def no-fspec (deref #'clojure.spec.test.alpha/no-fspec))
(def stacktrace-relevant-to-instrument (deref #'clojure.spec.test.alpha/stacktrace-relevant-to-instrument))
(def ^:private ^:dynamic *instrument-enabled*
  "if false, instrumented fns call straight through"
  true)

(defonce ^:private instrumented-vars (atom {}))

;; Macros

(defn- varargs [arglist]
  (let [[_ decl :as restargs] (->> arglist
                                   (split-with (complement #{'&}))
                                   second)]
    (and (= 2 (count restargs))
         decl)))

(defn- kwargs-context [v f fn-spec arglist decl]
  (let [as-name (or (:as decl) (gensym "as"))
        decl (assoc decl :as as-name)
        head-args (->> arglist (take-while (complement #{'&})) vec)]
    {:args    (conj head-args as-name)
     :data    `(->> ~as-name seq flatten (concat ~head-args))
     :arglist (-> arglist butlast vec (conj decl))
     :spec    fn-spec
     :var     v}))

(defn- varargs-context [v f fn-spec arglist decl]
  (if (map? decl)
    (kwargs-context v f fn-spec arglist decl)
    (let [head-args (->> arglist (take-while (complement #{'&})) vec)
          args-sym  'args]
      {:arglist (vec (concat head-args '[& args]))
       :data    `(list* ~@head-args ~args-sym)
       :args    `(list* ~@head-args ~args-sym)
       :spec    fn-spec
       :var     v})))

(defn- args-context [v f fn-spec arglist]
  {:args arglist
   :data arglist
   :spec fn-spec
   :var  v})

(defn- gen-body [context]
  `(if *instrument-enabled*
     (with-instrument-disabled
       (when (:args ~(:spec context))
         (conform! ~(:var context)                   ;; var
                   :args (:args ~(:spec context))    ;; spec object
                   ~(:data context)                  ;; data to check
                   ~(:args context)))                ;; args
       (binding [*instrument-enabled* true]
         (.applyTo ^clojure.lang.IFn kwargs-fn (seq ~(:args context))))) ;; seq of arglist
     (.applyTo ^clojure.lang.IFn kwargs-fn (seq ~(:args context)))))

(defn- fetch-spec [s]
  (@#'s/maybe-spec s))

(defn- gen-bodies [v f s]
  (let [arglists (->> v meta :arglists (sort-by count))
        v (if (var? v) v (resolve v))]
    (list `let ['fn-spec `(s/get-spec ~v)]
          (list* `fn
             (if (seq arglists)
              (map (fn [arglist]
                     (let [context (if-let [decl (varargs arglist)]
                                     (varargs-context v f 'fn-spec arglist decl)
                                     (args-context   v f 'fn-spec arglist))]
                       `(~(or (:arglist context) arglist)
                         ~(gen-body context))))
                   arglists)
              `(~'[& args]
                ~(gen-body {:args 'args
                            :data 'args
                            :var  v
                            :spec 'fn-spec})))))))

(comment

  (gen-bodies (var kwargs-fn) kwargs-fn `kwargs-fn)

)

(def conform!
  (fn [v role spec data args]
    (let [_ (println "chekcing " data " & " args)
          conformed (s/conform spec data)]
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
        conformed))))

(defmacro spec-checking-fn-macro
  [s]
  (let [v (resolve s)
        f @v]
    (gen-bodies v f s)))

(defmacro foo ([]))

(comment

  (def ff (spec-checking-fn-macro fogus.instr/kwargs-fn))

  (macroexpand-1 `(spec-checking-fn-macro fogus.instr/kwargs-fn))
  
  (ff 1)
  (ff 1 2)
  (ff 1 2 {:a 1})
  (ff 1 2 {:a 1 :b 2 :c 3})
  (ff 1 "b" {:a 1})
  (ff 1 2 {:a 1 :b "b"})

  (instrument '[fogus.instr/kwargs-fn fogus.instr/just-varargs] {})

  ;; expand to

  (locking instrumented-vars
    (when-let [v (resolve 'fogus.instr/kwargs-fn)]
      (when-not (-> v meta :macro)
        (let [opts {}
              spec (s/get-spec v)
              {:keys [raw wrapped]} (get @instrumented-vars v)
              current @v
              to-wrap (if (= wrapped current) raw current)
              ospec (or (instrument-choose-spec spec s opts)
                        (throw (no-fspec v spec)))
              ofn (instrument-choose-fn to-wrap ospec s opts)
              checked (let [fn-spec (get-spec ospec)]
                        (fn
                          ([opts]
                           (if *instrument-enabled*
                             (with-instrument-disabled
                               (when (:args fn-spec)
                                 (conform! v :args (:args fn-spec) [opts] [opts]))
                               (binding [*instrument-enabled* true]
                                 (.applyTo ofn (clojure.core/seq [opts]))))
                             (.applyTo ofn (seq [opts]))))

                          ([a b]
                           (if *instrument-enabled*
                             (with-instrument-disabled
                               (when (:args fn-spec)
                                 (conform! v :args (:args fn-spec) [a b] [a b]))
                               (binding [*instrument-enabled* true]
                                 (.applyTo ofn (seq [a b]))))
                             (.applyTo ofn (seq [a b]))))

                          ([a b & {:as m}]
                           (if *instrument-enabled*
                             (with-instrument-disabled
                               (when (:args fn-spec)
                                 (conform! v :args (:args fn-spec) (->> m seq flatten (concat [a b])) [a b m]))
                               (binding [*instrument-enabled* true]
                                 (.applyTo ofn (seq [a b m]))))
                             (.applyTo ofn (seq [a b m]))))))]
          (alter-var-root v (constantly checked))
          (swap! instrumented-vars assoc v {:raw to-wrap :wrapped checked}))))

    (when-let [v (resolve 'fogus.instr/just-varargs)]
      (when-not (-> v meta :macro)
        (let [opts {}
              spec (s/get-spec v)
              {:keys [raw wrapped]} (get @instrumented-vars v)
              current @v
              to-wrap (if (= wrapped current) raw current)
              ospec (or (instrument-choose-spec spec s opts)
                        (throw (no-fspec v spec)))
              ofn (instrument-choose-fn to-wrap ospec s opts)
              checked (let [fn-spec (get-spec ospec)]
                        (fn
                          ([& args]
                           (if *instrument-enabled*
                             (with-instrument-disabled
                               (when (:args fn-spec)
                                 (conform! v :args (:args fn-spec) args args))
                               (binding [*instrument-enabled* true]
                                 (.applyTo ofn (seq args))))
                             (.applyTo ofn (seq args))))))]
          (alter-var-root v (constantly checked))
          (swap! instrumented-vars assoc v {:raw to-wrap :wrapped checked}))))

    '[fogus.instr/kwargs-fn fogus.instr/just-varargs])
  

)
