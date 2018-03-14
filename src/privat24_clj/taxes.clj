(ns privat24-clj.taxes
  (:require [clj-time.core :as t]
            [privat24-clj.api.statement :as st]
            [privat24-clj.util :refer [remove-by-val]]
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

(defn- income? [{:keys [amount]}]
  (< 0 amount))

(defn- done? [{:keys [state]}]
  (= :done state))

(defn- real? [{:keys [type]}]
  (= :real type))

(defn credit-accounts-filter [credit-accounts statement]
  {:pre [(sequential? credit-accounts) (map? statement)]}
  (contains? (set (map str credit-accounts))
             (get statement :credit-account-number)))

(defn custom-filter [[k v]]
  (case k
    :credit-accounts (partial credit-accounts-filter (vec v))))

(defn add-statement-to-report [report {:keys [amount currency] :as statement}]
  (letfn [(add-amount [report]
            (update-in report
                       [:taxable-amount (keyword currency)]
                       #(if % (+ % amount) amount)))
          (add-statement [report]
                         (update report
                                 :statements
                                 #(conj % statement)))]
    (->> report
         add-amount
         add-statement)))

(defn make-tax-report [filters-map statements]
  (let [custom-filters (->> (remove-by-val empty? filters-map)
                            (map custom-filter))
        f (apply every-pred income? done? real? custom-filters)]
    (->> (filter f statements)
         (reduce add-statement-to-report {}))))
