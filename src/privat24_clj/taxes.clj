(ns privat24-clj.taxes
  (:require [clj-time.core :as t]
            [privat24-clj.api.statement :as st]
            [clj-time.format :as f]))

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

(defn current-quarter-range []
  (let [quarter (-> (last-quarters 1) first)]
    (map st/format-date quarter)))

(defn prev-quarter-range []
  (let [quarter (-> (last-quarters 2) last)]
    (map st/format-date quarter)))

(defn last-quarters-range [n]
  (let [quarters (last-quarters n)
        end-date (-> (first quarters) second)
        start-date (-> (last quarters) first)]
    [(st/format-date start-date)
     (st/format-date end-date)]))

(defn income? [statement]
  (< 0 (:amount statement)))

(defn done? [statement]
  (= :done (st/statement-state statement)))

(defn real? [statement]
  (= :real (st/statement-type statement)))

(defn custom-filter [[k v]]
  (let [credit-accounts-filter (fn [credit-accounts statement]
                                 (contains? (set credit-accounts)
                                            (get-in statement
                                                    [:credit :account (keyword "@number")])))]
    (case k
      :credit-accounts (partial credit-accounts-filter
                                (into [] v)))))

(defn add-statement-to-report [report {:keys [amount currency] :as statement}]
  (letfn [(add-amount [report]
            (update-in report
                       [:amount currency]
                       #(if % (+ % amount) amount)))
          (add-statement [report]
                         (update report
                                 :statements
                                 #(conj % (:raw statement))))]
    (->> report
         add-amount
         add-statement)))

(defn make-tax-report [filters-map statements]
  (let [f (apply every-pred income? done? real? (map custom-filter filters-map))]
    (->> (filter f statements)
         (reduce add-statement-to-report {}))))
