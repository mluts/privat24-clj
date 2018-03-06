(ns privat24-clj.util
  (:require [cheshire.core :as json]))

(defn bind-error [f [val err] & args]
  (if err [val err] (apply f val args)))

(defn ->json [data] (json/encode data))

(defn <-json [str] (json/decode str keyword))

(defn parse-float [str]
  (Float/parseFloat str))
