(ns fogus.spec
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]))

(defn foo [& {:keys [a b]}]
  [a b])

(s/def ::a any?)
(s/def ::b any?)

;; keys* not sufficient here
(s/fdef foo :args (s/keys* :opt [::a ::b]))

(s/def ::maps->map (s/conformer #(merge (:kvs %) (:trailing %))))

(defmacro keys*+
  "Initial form of a spec that accepts 1.11 like kwargs + trailing map.
   Open questions:
   - is an unformer possible?
   - is a generator needed?"
  [& kspecs]
  `(s/conformer
    #(->> % 
          (s/conform (s/cat :kvs (s/keys*) :trailing (s/? map?)))
          (s/conform ::maps->map)
          (s/conform (s/keys ~@kspecs)))))

(comment

  (s/conform (keys*2 :req-un [::a ::c])
             [:a 1 :c 2])
  
  (s/conform (keys*2 :req-un [::a ::c])
             [:a 1 :c 2 {:b 3}])

  (s/conform (keys*2 :req-un [::a ::c])
             [{:b 3}])

  (s/conform (keys*+ :req-un [::a ::c])
             [:a 1 :c 2])
  
  (s/conform (keys*+ :req-un [::a ::c])
             [:a 1 :c 2 {:b 3}])

  (s/conform (keys*+ :req-un [::a ::c])
             [{:a 1 :c 3}])

)

(defn add [& {:keys [a b]}] (+ a b))

(s/def ::a any?)
(s/def ::c any?)
(s/fdef add :args (keys*+ :opt [::a ::c]))

(comment
  (stest/instrument `add)
  
  (foo :a 1 :b 2)
  ;;=> [1 2]
  
  (foo {:a 1 :b 2})
  ;;=> [1 2]
  
  (stest/instrument `foo)
  
  (foo :a "a" :b "b")
  ;;=> ["a" "b"]
  
  (foo {:a "a" :b "b"})

  (add :a 1 :b 2)

  (add {:a 1 :b 2})

  (add :a 1 {:b 2})

  (defmacro qux [& {:keys [a]}]
    a)

  (stest/instrument `qux)
)

(defn- xform-keys*
  "Happy path keys* transformer. WiP"
  [[head & tail :as form]]
  (cond (= head 'clojure.spec.alpha/keys*) (list* 'keys*+ tail)
        (= head 'clojure.spec.alpha/cat)   (concat (list* head (butlast tail)) (let [[h & t] (last form)] [(list* 'keys*+ t)]))
        :default form))

(comment

  (xform-keys* `(s/keys* :opt-un [::a ::c]))

  (xform-keys* `(s/cat :arg1 int? :arg2 nil? :kwargs (s/keys* :opt-un [::a ::c])))

  (xform-keys* `(s/cat :kwargs (s/keys* :opt-un [::a ::c])))

  (get (s/registry) 'fogus.spec/add)
  
  (-> (s/registry)
      (get 'fogus.spec/add)
      (get :args)
      s/describe)

  @(deref #'clojure.spec.test.alpha/instrumented-vars)

  (meta #'foo)
)

(defn kwargs-fn
  ([opts] opts)
  ([a b] [a b])
  ([a b & {:as m}] [a b m]))

(defn no-kwargs-fn
  ([opts] opts)
  ([a b] [a b])
  ([a b & opts] [a b opts]))

(use 'clojure.spec.test.alpha)

(def instrument-choose-spec (deref #'clojure.spec.test.alpha/instrument-choose-spec))
(def instrument-choose-fn (deref #'clojure.spec.test.alpha/instrument-choose-fn))
(def no-fspec (deref #'clojure.spec.test.alpha/no-fspec))
(def stacktrace-relevant-to-instrument (deref #'clojure.spec.test.alpha/stacktrace-relevant-to-instrument))

(def ^:private ^:dynamic *instrument-enabled*
  "if false, instrumented fns call straight through"
  true)

(defonce ^:private instrumented-vars (atom {}))

(defn- args-table
  "Builds a map describing the arities found in a var's :arglists metadata. For strict arities
  the parameter count maps to the arglist. However, for variadic functions there are two
  possible mappings. First, the key :* maps to an arglist declared with named varargs
  or with a sequence destructuring form. On the other hand, the key :kwargs maps to an arglist
  declared as taking keyword arguments."
  [v]
  (let [arglists (->> v meta :arglists (sort-by count))]
    (reduce (fn [table al]
              (if (some #{'&} al)
                (let [[_ decl :as restargs] (->> al (split-with (complement #{'&})) second)]
                  (if (and (= 2 (count restargs))
                           (map? decl))
                    (assoc table :kwargs al)
                    (assoc table :* al)))
                (assoc table (count al) al)))
            {}
            arglists)))

(args-table #'kwargs-fn)
(args-table #'no-kwargs-fn)

(defn- spec-checking-fn
  [v f fn-spec]
  (let [fn-spec (@#'s/maybe-spec fn-spec)
        args-table (args-table v)
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
         (let [args' (cond (get args-table (count args)) args
                           (and (get args-table :kwargs)
                                (map? (last args)))      (concat (butlast args) (-> args last seq flatten))
                           :default                      args)]
           (when (:args fn-spec) (conform! v :args (:args fn-spec) args' args))
           (binding [*instrument-enabled* true]
             (.applyTo ^clojure.lang.IFn f args))))
       (.applyTo ^clojure.lang.IFn f args)))))

(defn- instrument-1
  [s opts]
  (when-let [v (resolve s)]
    (when-not (-> v meta :macro)
      (let [spec (s/get-spec v)
            {:keys [raw wrapped]} (get (deref #'clojure.spec.test.alpha/instrumented-vars) v)
            current @v
            to-wrap (if (= wrapped current) raw current)
            ospec (or (instrument-choose-spec spec s opts)
                      (throw (no-fspec v spec)))
            ofn (instrument-choose-fn to-wrap ospec s opts)
            checked (spec-checking-fn v ofn ospec)]
        (alter-var-root v (constantly checked))
        (swap! instrumented-vars assoc v {:raw to-wrap :wrapped checked})
        (->sym v)))))

(comment

  (defn plus+ [& {:keys [a c]}] (+ a c))

  (s/def ::a any?)
  (s/def ::c any?)
  (s/fdef plus+ :args (s/keys* :opt [::a ::c]))

  (instrument-1 `plus+ {})

  (plus+ :a 1 :c 2)
  (plus+ :a 1 {:c 2})
  (plus+ {:a 1 :c 2})
)

;;; Experiments below

(defn- has-kwargs? [v]
  (let [[_ decl :as restargs] (->> v
                                   meta
                                   :arglists
                                   (sort-by count)
                                   last
                                   (split-with (complement #{'&}))
                                   second)]
    (and (= 2 (count restargs))
         (map? decl))))

(has-kwargs? #'kwargs-fn)
(has-kwargs? #'no-kwargs-fn)


(defn- apply-kwargs [f & args]
  (let [args+ (concat (butlast args) (-> args last seq flatten))]
    (.applyTo ^clojure.lang.IFn f args+)))

;;(apply-kwargs bar 1 2 {:a 1 :b 2})

(def thunk (fn
             ([opts] (kwargs-fn opts))
             ([a b]  (kwargs-fn a b))
             ([a b & {:as m}]
              (apply kwargs-fn a b (-> m seq flatten)))))

(thunk {:a 1})
(thunk 1 2)
(thunk 1 2 {:a 1 :b 2})

