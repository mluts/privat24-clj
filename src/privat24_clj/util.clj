(ns privat24-clj.util
  (:require [cheshire.core :as json]))

(defn ->json [data] (json/encode data))

(defn <-json [str] (json/decode str keyword))

(defn parse-float [str]
  (Float/parseFloat str))

(defn remove-by-val [pred m]
  (into {} (remove (comp pred val) m)))

(defn map-vals [pred m]
  (into {} (map (juxt key (comp pred val)) m)))
