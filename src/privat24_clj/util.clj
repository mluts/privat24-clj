(ns privat24-clj.util
  (:require [cheshire.core :as json]))

(defn ->json [data] (json/encode data))

(defn <-json [str] (json/decode str keyword))

(defn parse-float [str]
  (Float/parseFloat str))
