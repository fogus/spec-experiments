(ns instrument-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [fogus.instr :as stest]
            [clojure.spec.gen.alpha :as gen]
            clojure.spec.test.alpha))

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

(comment
  (stest/instrument-local `kwargs-fn {})
  (stest/unstrument-local `kwargs-fn)
  (clojure.spec.test.alpha/check `kwargs-fn)

  (kwargs-fn 1 :a)
  (kwargs-fn 1 2 {:b :a})
)

;;; Tests

(deftest test-instrument
  (testing "that a function taking fixed args and varargs is spec'd and checked at runtime"
    (letfn [(test-varargs-raw []
              (are [x y] (= x y)
                1                         (no-kwargs-fn 1)
                [1 2]                     (no-kwargs-fn 1 2)
                [1 2 [3 4 5]]             (no-kwargs-fn 1 2 3 4 5)))]
      (testing "that the raw kwargs function operates as expected"
        (test-varargs-raw))

      (testing "that the instrumented kwargs function operates as expected"
        (stest/instrument-local `no-kwargs-fn {})
        (stest/instrument-local `just-varargs {})

        (test-varargs-raw)
        (is (clojure.spec.test.alpha/check `no-kwargs-fn) (-> first :clojure.spec.test.check/ret :pass?))
        (is (clojure.spec.test.alpha/check `just-varargs) (-> first :clojure.spec.test.check/ret :pass?))

        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"did not conform to spec" (no-kwargs-fn 1 :not-num)))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"did not conform to spec" (no-kwargs-fn 1 2 :not-num 3))))

      (testing "that the uninstrumented kwargs function operates as the raw function"
        (stest/unstrument-local `no-kwargs-fn)
        (test-varargs-raw))))
  
  (testing "that a function taking keyword args is spec'd and checked at runtime"
    (letfn [(test-kwargs-baseline []
              (are [x y] (= x y)
                1                         (kwargs-fn 1)
                [1 2]                     (kwargs-fn 1 2)
                [1 2 {:a 1}]              (kwargs-fn 1 2 :a 1)
                [1 2 {:a 1}]              (kwargs-fn 1 2 {:a 1})
                [1 2 {:a 1 :b 2}]         (kwargs-fn 1 2 :a 1 {:b 2})))
            (test-kwargs-extended []
              (are [x y] (= x y)
                [1 :not-num]              (kwargs-fn 1 :not-num)
                [1 2 {:a 1 :b :not-num}]  (kwargs-fn 1 2 :a 1 {:b :not-num})))]
      (testing "that the raw kwargs function operates as expected"
        (test-kwargs-baseline)
        (test-kwargs-extended))

      (testing "that the instrumented kwargs function operates as expected"
        (stest/instrument-local `kwargs-fn {})

        (test-kwargs-baseline)
        (is (clojure.spec.test.alpha/check `kwargs-fn) (-> first :clojure.spec.test.check/ret :pass?))

        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"did not conform to spec" (kwargs-fn 1 :not-num)))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"did not conform to spec" (kwargs-fn 1 2 :a 1 {:b :not-num}))))

      (testing "that the uninstrumented kwargs function operates as the raw function"
        (stest/unstrument-local `kwargs-fn)
        (test-kwargs-baseline)
        (test-kwargs-extended))))

  (testing "that a var with no arglists meta is spec'd and checked at runtime"
    (is (clojure.spec.test.alpha/check `add10) (-> first :clojure.spec.test.check/ret :pass?))
    
    (stest/instrument-local `add10 {})
    (is (clojure.spec.test.alpha/check `add10) (-> first :clojure.spec.test.check/ret :pass?))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"did not conform to spec" (add10 :not-num)))
    
    (stest/unstrument-local `kwargs-fn)
    (is (clojure.spec.test.alpha/check `add10) (-> first :clojure.spec.test.check/ret :pass?))))
