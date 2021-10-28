(ns fogus.mac
  (:require [clojure.spec.alpha :as s]))

(def my-ver {:major 1 :minor #(>= % 11)})

(s/def :trailing-map/major #{1})
(s/def :trailing-map/minor #(>= % 11))
(s/def :trailing-map/clojure-version (s/keys :opt-un [:trailing-map/major :trailing-map/minor]))

(s/valid? :trailing-map/clojure-version *clojure-version*)

(defmacro against-version [condition & body]
  `(when (s/valid? ~condition *clojure-version*)
     ~@body))

(macroexpand-1 '(against-version (s/valid? :trailing-map/clojure-version *clojure-version*) 42))

(against-version
 :trailing-map/clojure-version
 42)

