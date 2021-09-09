(ns fogus.spec
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]))

(comment

(defn foo [& {:keys [a b]}] [a b])

(foo :a 1 :b 2)
;;=> [1 2]

(foo {:a 1 :b 2})
;;=> [1 2]

(s/def ::a any?)
(s/def ::b any?)
(s/fdef foo :args (s/keys* :opt [::a ::b]))

(stest/instrument `foo)

(foo :a "a" :b "b")
;;=> ["a" "b"]

(foo {:a "a" :b "b"})

)
