(ns privat24-clj.test.util
  (:require [privat24-clj.api.statement :as st]
            [privat24-clj.util :refer [<-json]]))

(defn statements-example []
  (->> (clojure.java.io/resource "business-statements.json")
       slurp
       <-json
       (map st/parse-row)))
