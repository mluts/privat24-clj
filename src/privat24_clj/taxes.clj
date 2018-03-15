(ns privat24-clj.taxes
  (:require [clj-time.core :as t]
            [privat24-clj.api.statement :as st]
            [medley.core :as m]
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

(defn- date [{:keys [datetime]}]
  (when datetime
    (f/unparse (f/formatters :date) datetime)))

(defn- week [{:keys [datetime]}]
  (when datetime
    (f/unparse (f/formatters :weekyear-week) datetime)))

(defn- month [{:keys [datetime]}]
  (when datetime
    (f/unparse (f/formatters :year-month) datetime)))

(defn credit-accounts-filter [credit-accounts statement]
  {:pre [(sequential? credit-accounts) (map? statement)]}
  (contains? (set (map str credit-accounts))
             (get statement :credit-account-number)))

(defn custom-filter [[k v]]
  (case k
    :credit-accounts (partial credit-accounts-filter (vec v))))

(defn add-statement-to-report [report {:keys [amount currency inferred-currency-rate] :as statement}]
  (letfn [(convertable-currencies [] (get inferred-currency-rate (keyword currency)))
          (add-to [a] (fn [b] (if b (+ a b) a)))
          (add-amount [report]
                      (update-in report
                                 [:taxable-amount (keyword currency)]
                                 (add-to amount)))
          (add-convertable-amounts [report]
                                   (reduce (fn [rep [cur-name rate]]
                                             (update-in rep
                                                        [:taxable-amount (keyword cur-name)]
                                                        (add-to (* rate amount))))
                                           report
                                           (convertable-currencies)))
          (add-statement [report]
                         (update report
                                 :statements
                                 #(conj % statement)))]
    (->> report
         add-amount
         add-convertable-amounts
         add-statement)))

(defn inject-currency-rates [statements]
  (let [aggregate-currency-rates  (fn [statements]
                                    (transduce (comp (map :detected-currency-rate)
                                                     (filter identity)
                                                     (map vector)
                                                     cat)
                                               merge {} statements))
        rates-by-date (m/map-vals aggregate-currency-rates (group-by date statements))
        rates-by-week (m/map-vals aggregate-currency-rates (group-by week statements))
        rates-by-month (m/map-vals aggregate-currency-rates (group-by month statements))
        first-rates (aggregate-currency-rates statements)
        any-rates (fn [statement]
                    (let [d (date statement)
                          w (week statement)
                          m (month statement)
                          day-rates (get rates-by-date d)
                          week-rates (get rates-by-week w)
                          month-rates (get rates-by-month m)]
                      (merge first-rates month-rates week-rates day-rates)))]
    (map #(assoc % :inferred-currency-rate (any-rates %)) statements)))

(defn make-tax-report [{:keys [date-start date-end name filters-map]} statements]
  (let [custom-filters (map custom-filter (m/remove-vals empty? filters-map))
        filters (apply every-pred income? done? real? custom-filters)]
    (transduce (filter filters)
               (completing add-statement-to-report)
               {:date-start date-start
                :date-end date-end
                :name name}
               (inject-currency-rates statements))))
