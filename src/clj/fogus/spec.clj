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
(stest/instrument `add)

(comment

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

)
