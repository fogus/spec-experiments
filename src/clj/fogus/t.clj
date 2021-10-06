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

(defonce ^:private instrumented-vars (atom {}))

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

;; spec utils

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

;;; macro utils

(defn- varargs [arglist]
  (let [[_ decl :as restargs] (->> arglist
                                   (split-with (complement #{'&}))
                                   second)]
    (and (= 2 (count restargs))
         decl)))

(defn- kwargs-context [arglist decl]
  (let [as-name (or (:as decl) (gensym "as"))
        decl (assoc decl :as as-name)
        head-args (->> arglist (take-while (complement #{'&})) vec)]
    {:args    (conj head-args as-name)
     :data    `(->> ~as-name seq flatten (concat ~head-args))
     :arglist (-> arglist butlast vec (conj decl))}))

(defn- varargs-context [arglist decl]
  (if (map? decl)
    (kwargs-context arglist decl)
    (let [head-args (->> arglist (take-while (complement #{'&})) vec)
          args-sym  'args]
      {:arglist (vec (concat head-args '[& args]))
       :data    `(list* ~@head-args ~args-sym)
       :args    `(list* ~@head-args ~args-sym)})))

(defn- args-context [arglist]
  {:args    arglist
   :data    arglist
   :arglist arglist})

(defn- gen-body [context]
  `(if *instrument-enabled*
     (with-instrument-disabled
       (when (:args ~(:spec context))
         (conform! ~(:var context)                   ;; var
                   :args (:args ~(:spec context))    ;; spec object
                   ~(:data context)                  ;; data to check
                   ~(:args context)))                ;; args
       (binding [*instrument-enabled* true]
         (.applyTo ^clojure.lang.IFn ~(:fun context) (seq ~(:args context))))) ;; seq of arglist
     (.applyTo ^clojure.lang.IFn ~(:fun context) (seq ~(:args context)))))

(defn- fetch-spec [s]
  (@#'s/maybe-spec s))

(defn- gen-bodies [s v {:keys [FUN SPEC] :as context}]
  (map (fn [arglist]
         (let [context (if-let [decl (varargs arglist)]
                         (varargs-context arglist decl)
                         (args-context    arglist))]           
           (list arglist
                 (gen-body (merge context {:var v :fun FUN :spec SPEC})))))
       (or (->> v meta :arglists (sort-by count))
           '([& args]))))

(defn- gen-thunk [s v f opts]
  (let [FUN  (gensym "ofn")
        SPEC (gensym "ospec")]
    `(let [spec# (s/get-spec ~v)
           ~SPEC (or (instrument-choose-spec spec# ~s ~opts)
                     (throw (no-fspec ~v spec#)))
           ~FUN (instrument-choose-fn ~f ~SPEC ~s ~opts)]
       (fn ~@(gen-bodies s v {:FUN FUN, :SPEC SPEC})))))

(defn- instrument-1
  [s opts]
  (when-let [v (resolve s)]
    (when-not (-> v meta :macro)
      (let [{:keys [raw wrapped]} (get @instrumented-vars v)
            current @v
            to-wrap (if (= wrapped current) raw current)
            checked (gen-thunk s v to-wrap opts)]
        (alter-var-root v (constantly (eval checked)))
        (swap! instrumented-vars assoc v {:raw to-wrap :wrapped checked})
        (->sym v)))))

(comment
  ((->
    (instrument-1 `kwargs-fn {})
    eval)
   1 2 :a 1)

  (kwargs-fn 1)
  (kwargs-fn 1 2)
  (kwargs-fn 1 2 :a 1)
  (kwargs-fn 1 2 :a 1 {:b 2})
  (kwargs-fn 1 :B)
  
  ((instrument-1 `kwargs-fn {}) 1)
  ((instrument-1 `kwargs-fn {}) 1 2)
  ((instrument-1 `kwargs-fn {}) 1 2 {:a 1})

  (instrument-1 `no-kwargs-fn {})
  (instrument-1 `just-varargs {})


  (def f
    (let [opts {}
          v #'kwargs-fn
          
          {:keys [raw wrapped]} (get @instrumented-vars v)
          current @v
          to-wrap (if (= wrapped current) raw current)
          spec (s/get-spec v)
          OSPEC (or (instrument-choose-spec spec s opts)
                    (throw (no-fspec v spec)))
          OFN (instrument-choose-fn to-wrap OSPEC s opts)
          checked (fn
                    ([opts]
                     (if *instrument-enabled*
                       (with-instrument-disabled
                         (when (:args OSPEC)
                           (conform! v :args (:args OSPEC) [opts] [opts]))
                         (binding [*instrument-enabled* true]
                           (.applyTo OFN (clojure.core/seq [opts]))))
                       (.applyTo OFN (seq [opts]))))
                    
                    ([a b]
                     (if *instrument-enabled*
                       (with-instrument-disabled
                         (when (:args OSPEC)
                           (conform! v :args (:args OSPEC) [a b] [a b]))
                         (binding [*instrument-enabled* true]
                           (.applyTo OFN (seq [a b]))))
                       (.applyTo OFN (seq [a b]))))
                    
                    ([a b & {:as m}]
                     (if *instrument-enabled*
                       (with-instrument-disabled
                         (when (:args OSPEC)
                           (conform! v :args (:args OSPEC) (->> m seq flatten (concat [a b])) [a b m]))
                         (binding [*instrument-enabled* true]
                           (.applyTo OFN (seq [a b m]))))
                       (.applyTo OFN (seq [a b m])))))]
      checked))

  (f 1)
  (f 1 2)
  (f 1 2 :a 1)
  (f 1 2 {:a 1})
  (f 1 :B)
  (f 1 2 {:a 1 :b :B})

  (def f
    (eval
     '(clojure.core/let [{:keys [raw__10996__auto__ wrapped__10997__auto__]} (clojure.core/get (clojure.core/deref fogus.t/instrumented-vars) #'fogus.t/kwargs-fn)
                         current__10998__auto__ (clojure.core/deref #'fogus.t/kwargs-fn)
                         to-wrap__10999__auto__ (if (clojure.core/= wrapped__10997__auto__ current__10998__auto__) raw__10996__auto__ current__10998__auto__)
                         spec__11000__auto__ (clojure.spec.alpha/get-spec #'fogus.t/kwargs-fn)
                         ospec11007 (clojure.core/or (fogus.t/instrument-choose-spec spec__11000__auto__ fogus.t/kwargs-fn {})
                                                     (throw (fogus.t/no-fspec #'fogus.t/kwargs-fn spec__11000__auto__)))
                         ofn11006 (fogus.t/instrument-choose-fn to-wrap__10999__auto__ ospec11007 fogus.t/kwargs-fn {})]
        (clojure.core/fn
          ([opts]
           (if fogus.t/*instrument-enabled*
             (clojure.spec.test.alpha/with-instrument-disabled
               (clojure.core/when (:args ospec11007)
                 (fogus.t/conform! #'fogus.t/kwargs-fn
                                   :args (:args ospec11007)
                                   [opts]
                                   [opts]))
               (clojure.core/binding [fogus.t/*instrument-enabled* true]
                 (.applyTo fogus.t/kwargs-fn (clojure.core/seq [opts]))))
             (.applyTo fogus.t/kwargs-fn (clojure.core/seq [opts]))))

          ([a b]
           (if fogus.t/*instrument-enabled*
             (clojure.spec.test.alpha/with-instrument-disabled
               (clojure.core/when (:args ospec11007)
                 (fogus.t/conform! #'fogus.t/kwargs-fn
                                   :args (:args ospec11007)
                                   [a b]
                                   [a b]))
               (clojure.core/binding [fogus.t/*instrument-enabled* true]
                 (.applyTo fogus.t/kwargs-fn (clojure.core/seq [a b]))))
             (.applyTo fogus.t/kwargs-fn (clojure.core/seq [a b]))))

          ([a b & {:as m}]
           (if fogus.t/*instrument-enabled*
             (clojure.spec.test.alpha/with-instrument-disabled
               (clojure.core/when (:args ospec11007)
                 (fogus.t/conform! #'fogus.t/kwargs-fn
                                   :args (:args ospec11007)
                                   (clojure.core/->> m clojure.core/seq clojure.core/flatten (clojure.core/concat [a b]))
                                   [a b m]))
               (clojure.core/binding [fogus.t/*instrument-enabled* true]
                 (.applyTo fogus.t/kwargs-fn (clojure.core/seq [a b m]))))
             (.applyTo fogus.t/kwargs-fn (clojure.core/seq [a b m]))))))))

  (f 1)
  (f 1 2)
  (f 1 2 :a 1)
  (f 1 2 {:a 1})
  (f 1 :B)
  (f 1 2 {:a 1 :b :B})

)
