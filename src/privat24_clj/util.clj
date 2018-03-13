(ns privat24-clj.util
  (:require [cheshire.core :as json]
            [cheshire.generate :refer [add-encoder encode-str]])
  (:import [ord.joda.time.DateTime]))

(defn ->json
  ([data opts] (json/encode data opts))
  ([data] (->json data {})))

(defn <-json [str] (json/decode str keyword))

(defn parse-float [str]
  (Float/parseFloat str))

(defn remove-by-val [pred m]
  (into {} (remove (comp pred val) m)))

(defn map-vals [pred m]
  (into {} (map (juxt key (comp pred val)) m)))

(add-encoder org.joda.time.DateTime encode-str)
