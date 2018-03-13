(ns privat24-clj.api.statement
  (:require [privat24-clj.util :as util]
            [privat24-clj.util :as util]
            [clj-time.format :as f]))

(defn date-formatter []
  (f/formatter "dd.MM.yyyy"))

(defn format-date [date-time]
  (f/unparse (date-formatter) date-time))

(defn statement-post-date-formatter []
  (f/formatter "yyyyMMdd'T'HH:mm:ss"))

(defn detect-currency-rate [row]
  (let [detect-by-purpose (fn [row]
                            (let [m (re-matches #"(по\s+курсу\s+)(\d+\.\d+)" (str (:purpose row)))]
                              (when m
                                (util/parse-float (last m)))))]
    (->> (map #(% row) [detect-by-purpose])
         (filter identity)
         first)))

(defn- statement-state [statement]
  (case (get-in statement [:info (keyword "@state")])
    "r" :done
    "t" :rollback
    nil))

(defn- statement-type [statement]
  (case (get-in statement [:info (keyword "@flinfo")])
    "r" :real
    "i" :info
    nil))

(defn parse-row [row]
  {:amount (-> (get-in row [:amount (keyword "@amt")])
               (util/parse-float))
   :currency (get-in row [:amount (keyword "@ccy")])
   :purpose (:purpose row)
   :detected-currency-rate (detect-currency-rate row)
   :datetime (when-let [post-date (get-in row [:info (keyword "@postdate")])]
               (f/parse (statement-post-date-formatter) post-date))
   :credit-account-number (get-in row [:credit :account (keyword "@number")])
   :id (get-in row [:info (keyword "@refp")])
   :state (statement-state row)
   :type (statement-type row)})
