(ns fogus.spec
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]))

(defn foo [& {:keys [a b]}]
  [a b])

(s/def ::a any?)
(s/def ::b any?)

(s/fdef foo :args (s/keys* :opt [::a ::b]))
;; keys* not sufficient here

(def kvs<-map #(map (fn [[k v]] {::k k ::v v}) %))

(s/def ::kvs->map (s/conformer #(zipmap (map ::k %) (map ::v %)) kvs<-map))
(s/def ::lift-map (s/conformer identity list))
(s/def ::kvs->map (s/conformer #(zipmap (map ::k %) (map ::v %)) #(map (fn [[k v]] {::k k ::v v}) %)))

(defmacro kvs+tm [& kspecs]
  `(let [mspec# (s/keys ~@kspecs)]
     (s/with-gen (clojure.spec.alpha/&
                  (s/alt :m (clojure.spec.alpha/& mspec# ::lift-map)
                         :s (clojure.spec.alpha/& (s/* (s/cat ::k keyword? ::v any?)) ::kvs->map mspec#))
                  (s/conformer second #(map (fn [[k# v#]] {::k k# ::v v#}) %)))
       (fn [] (gen/fmap (fn [m#] (apply concat m#)) (s/gen mspec#))))))

(defmacro keys*
  [& kspecs]
  `(let [mspec# (s/keys ~@kspecs)]
     (s/with-gen (s/& (s/* (s/cat ::k keyword? ::v any?)) ::kvs->map mspec#)
       (fn [] (gen/fmap (fn [m#] (apply concat m#)) (s/gen mspec#))))))

(defmacro keys*2
  [& kspecs]
  `(s/cat :kvs (s/keys* ~@kspecs)
          :trailing (s/? map?)))

(comment

  (s/conform (keys*2 :req-un [::a ::c])
             [:a 1 :c 2 {:b 3}])
  
  (s/conform (keys* :req-un [::a ::c])
             [:a 1 :c 2])

  (s/conform ::TM [:a 1 :b 2 {:c 3}])

)

(defn add [& {:keys [a b]}] (+ a b))

(s/def ::a any?)
(s/def ::c any?)
(s/fdef add :args (kvs+tm :opt [::a ::c]))
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
