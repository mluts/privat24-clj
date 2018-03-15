(ns privat24-clj.api.statement
  (:require [privat24-clj.util :as util]
            [clj-time.format :as f]))

(defn date-formatter []
  (f/formatter "dd.MM.yyyy"))

(defn format-date [date-time]
  (f/unparse (date-formatter) date-time))

(defn statement-post-date-formatter []
  (f/formatter "yyyyMMdd'T'HH:mm:ss"))

(defn detect-currency-rate [row]
  (letfn [(detect-by-purpose [{:keys [purpose]}]
            (let [[_ rate] (re-find #"продажу.+?USD.+?по\s+курсу\s+(\d+\.\d+)" (str purpose))]
              (when rate
                {:USD {:UAH (util/parse-double rate)}})))]
    (detect-by-purpose row)))

(defn- statement-state [{{state (keyword "@state")} :info}]
  (case state
    "r" :done
    "t" :rollback
    nil))

(defn- statement-type [{{type (keyword "@flinfo")} :info}]
  (case type
    "r" :real
    "i" :info
    nil))

(defn parse-row [{{amount (keyword "@amt")
                   currency (keyword "@ccy")} :amount
                  {post-date (keyword "@postdate")
                   id (keyword "@refp")} :info
                  purpose :purpose
                  {{credit-account-number (keyword "@number")} :account} :credit
                  :as row}]
  {:amount (util/parse-double amount)
   :currency currency
   :purpose purpose
   :detected-currency-rate (detect-currency-rate row)
   :datetime (when post-date (f/parse (statement-post-date-formatter) post-date))
   :credit-account-number credit-account-number
   :id id
   :state (statement-state row)
   :type (statement-type row)})
