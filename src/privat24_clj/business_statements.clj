(ns privat24-clj.business-statements
  (:require [privat24-clj.util :as util]
            [privat24-clj.taxes :as tax]
            [privat24-clj.util :as util]
            [clj-time.format :as f]))

(defn- date-formatter []
  (f/formatter "dd.MM.yyyy"))

(defn- format-date [date-time]
  (f/unparse (date-formatter) date-time))

(defn detect-currency-rate [row]
  (let [detect-by-purpose (fn [row]
                            (let [m (re-matches #"(по\s+курсу\s+)(\d+\.\d+)" (str (:purpose row)))]
                              (when m
                                (util/parse-float (last m)))))]
    (->> (map #(% row) [detect-by-purpose])
         (filter identity)
         first)))

(defn parse-row [row]
  {:amount (-> (get-in row [:amount (keyword "@amt")])
               (util/parse-float))
   :currency (get-in row [:amount (keyword "@ccy")])
   :purpose (:purpose row)
   :detected-currency-rate (detect-currency-rate row)
   :raw row})

(defn current-quarter-range []
  (let [quarter (-> (tax/last-quarters 1) first)]
    (map format-date quarter)))

(defn prev-quarter-range []
  (let [quarter (-> (tax/last-quarters 2) last)]
    (map format-date quarter)))

(defn last-quarters-range [n]
  (let [quarters (tax/last-quarters n)
        end-date (-> (first quarters) second)
        start-date (-> (last quarters) first)]
    [(format-date start-date)
     (format-date end-date)]))
