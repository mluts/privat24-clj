(ns privat24-clj.util
  (:require [cheshire.core :as json]
            [cheshire.generate :refer [add-encoder encode-str]]))

(defn ->json
  ([data opts] (json/encode data opts))
  ([data] (->json data {})))

(defn <-json [str] (json/decode str keyword))

(defn parse-double [str]
  (Double/parseDouble str))

(add-encoder org.joda.time.DateTime encode-str)
