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


