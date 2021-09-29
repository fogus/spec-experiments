(ns fogus.mac
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

;;; Pull in instrument stuff
(use 'clojure.spec.test.alpha)
(def instrument-choose-spec (deref #'clojure.spec.test.alpha/instrument-choose-spec))
(def instrument-choose-fn (deref #'clojure.spec.test.alpha/instrument-choose-fn))
(def no-fspec (deref #'clojure.spec.test.alpha/no-fspec))
(def stacktrace-relevant-to-instrument (deref #'clojure.spec.test.alpha/stacktrace-relevant-to-instrument))
(def ^:private ^:dynamic *instrument-enabled*
  "if false, instrumented fns call straight through"
  true)

(defonce ^:private instrumented-vars (atom {}))

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

(let [fn-spec (or (instrument-choose-spec (s/get-spec #'kwargs-fn) 'kwargs-fn {})
                  (throw (no-fspec #'kwargs-fn (s/get-spec #'kwargs-fn))))]
  (s/conform (:args fn-spec) [1 2 :a 42 :c 13]))

;; This is what should be generated
(def checked-kwargs-fn
  (let [fn-spec (or (instrument-choose-spec (s/get-spec #'kwargs-fn) 'kwargs-fn {})
                    (throw (no-fspec #'kwargs-fn (s/get-spec #'kwargs-fn))))
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
      ([opts]   ;; GEN BODY-1
       (if *instrument-enabled*
         (with-instrument-disabled
           (when (:args fn-spec) (conform! #'kwargs-fn             ;; var
                                           :args (:args fn-spec)   ;; spec object
                                           [opts]                  ;; arglist
                                           [opts]))                ;; arglist
           (binding [*instrument-enabled* true]
             (.applyTo ^clojure.lang.IFn kwargs-fn (seq [opts])))) ;; seq of arglist
         (.applyTo ^clojure.lang.IFn kwargs-fn (seq [opts]))))     ;; seq of arglist
      ([a b]
       (if *instrument-enabled*
         (with-instrument-disabled
           (when (:args fn-spec) (conform! #'kwargs-fn
                                           :args (:args fn-spec)
                                           [a b]
                                           [a b]))
           (binding [*instrument-enabled* true]
             (.applyTo ^clojure.lang.IFn kwargs-fn (seq [a b]))))
         (.applyTo ^clojure.lang.IFn kwargs-fn (seq [a b]))))
      ([a b & {:as m}]  ;; arglist with ammended :as (if needed)
       (if *instrument-enabled*
         (with-instrument-disabled
           (when (:args fn-spec) (conform! #'kwargs-fn
                                           :args (:args fn-spec)
                                           (concat [a b] (-> m seq flatten)) ;; flatten the map m 
                                           [a b m])) ;; ??? can't get the actual call form
           (binding [*instrument-enabled* true]
             (.applyTo ^clojure.lang.IFn kwargs-fn (seq [a b m]))))
         (.applyTo ^clojure.lang.IFn kwargs-fn (seq [a b m])))))))

(checked-kwargs-fn 1)
(checked-kwargs-fn 1 2)
(checked-kwargs-fn 1 2 {:a 42 :c 108})
;;(checked-kwargs-fn 1 "a" {:a 42 :c 108}) ;; fails as expected
;;(checked-kwargs-fn 1 2 {:a 42 :b "a" :c 108}) ;; fails but in weird way

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

(def fspc (or (instrument-choose-spec (s/get-spec #'kwargs-fn) 'kwargs-fn {}) (throw (no-fspec #'kwargs-fn (s/get-spec #'kwargs-fn)))))

(gen-body {:data '[opts] :args '[opts] :var #'kwargs-fn :spec fspc})

(let [pre '[a b]
      anm 'm]
  (gen-body {:data `(concat ~pre (-> ~anm seq flatten)) :args '[a b m] :var #'kwargs-fn :spec fspc}))

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
    {:arglist '[& args]
     :data    'args
     :args    'args
     :spec    fn-spec
     :var     v}))

(defn- args-context [v f fn-spec arglist]
  {:args arglist
   :data arglist
   :spec fn-spec
   :var  v})

(defn- gen-bodies [v f fn-spec]
  (let [arglists (->> v meta :arglists (sort-by count))]
    (list* `fn
           (if (seq arglists)
             (map (fn [arglist]
                    (let [context (if-let [decl (varargs arglist)]
                                    (varargs-context v f fn-spec arglist decl)
                                    (args-context   v f fn-spec arglist))]
                      `(~(or (:arglist context) arglist)
                        ~(gen-body context))))
                  arglists)
             `(~'[& args]
               ~(gen-body {:args 'args
                           :data 'args
                           :var  v
                           :spec fn-spec}))))))

(varargs '[& args])
(varargs '[& {:as m}])
(varargs '[& [head & tail]])
(gen-body {:args 'args :data 'args :var #'kwargs-fn :spec fspc})
(gen-bodies #'kwargs-fn kwargs-fn fspc)

;; TODO: handle varargs case
;; TODO: error handling
;; TODO: modify spec-checking-fn to include gen-bodies

(comment

  ;; Generated thunk
  (fn
    ([opts]
     (if *instrument-enabled*
       (with-instrument-disabled
         (when (:args 'ARGS_SPEC_OBJECT)
           (conform! #'fogus.mac/kwargs-fn
                     :args (:args 'ARGS_SPEC_OBJECT)
                     [opts]
                     [opts]))
         (binding [*instrument-enabled* true]
           (.applyTo fogus.mac/kwargs-fn (seq [opts]))))
       (.applyTo fogus.mac/kwargs-fn (seq [opts]))))
    ([a b]
     (if *instrument-enabled*
       (with-instrument-disabled
         (when (:args 'ARGS_SPEC_OBJECT)
           (conform! #'fogus.mac/kwargs-fn
                     :args (:args 'ARGS_SPEC_OBJECT)
                     [a b]
                     [a b]))
         (binding [*instrument-enabled* true]
           (.applyTo fogus.mac/kwargs-fn (seq [a b]))))
       (.applyTo fogus.mac/kwargs-fn (seq [a b]))))

    ([a b & {:as m}]
     (if *instrument-enabled*
       (with-instrument-disabled
         (when (:args 'ARGS_SPEC_OBJECT)
           (conform! #'fogus.mac/kwargs-fn
                     :args (:args 'ARGS_SPEC_OBJECT)
                     (->> m seq flatten (concat [a b]))
                     [a b m]))
         (binding [*instrument-enabled* true]
           (.applyTo fogus.mac/kwargs-fn (seq [a b m]))))
       (.applyTo fogus.mac/kwargs-fn (seq [a b m])))))
)
