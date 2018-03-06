(ns privat24-clj.business-statements
  (:require [privat24-clj.util :as util]
            [clj-time.format :as f]))

(defn date-formatter []
  (f/formatter "dd.MM.yyyy"))

(defn format-date [date-time]
  (f/unparse (date-formatter) date-time))

(defn parse-row [row]
  {:amount (-> (get-in row [:amount (keyword "@amt")])
               (util/parse-float))
   :currency (get-in row [:amount (keyword "@ccy")])
   :purpose (:purpose row)
   :raw row})
