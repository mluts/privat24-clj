(ns privat24-clj.taxes
  (:require [clj-time.core :as t]))

(def ^:const ukraine-quarter-months
  [[1 3]
   [4 6]
   [7 9]
   [10 12]])

(def ^:const ukraine-quarter-tax-percent 5)

(defn last-quarters
  ([n]
   {:pre [(int? n)]}
   (last-quarters (t/now) n))

  ([date n]
   {:pre [(int? n)
          (= org.joda.time.DateTime
             (type date))]}
   (if (> n 0)
     (let [contains-month (fn [[left right]] (<= left (t/month date) right))
           [start-month end-month] (-> (filter contains-month ukraine-quarter-months) first)
           start-date (t/first-day-of-the-month (t/year date) start-month)
           end-date (t/last-day-of-the-month (t/year date) end-month)
           prev-end-date (t/minus- start-date (t/months 1))]
       (conj (last-quarters prev-end-date (dec n))
             [start-date end-date]))
     nil)))
