(ns privat24-clj.taxes-test
  (:require [privat24-clj.taxes :refer :all]
            [privat24-clj.api.statement :as st]
            [privat24-clj.util :refer [<-json]]
            [clj-time.core :as t]
            [clojure.test :refer :all]
            [privat24-clj.test.util :refer [statements-example]]))

(deftest make-tax-report-test
  (testing "it filters by credit-accounts"
    (let [statements (statements-example)
          statements-map (into {} (map (fn [{:keys [id] :as m}] [id m]) statements))
          usd-uah 28.47
          report1 (make-tax-report {:filters-map {:credit-accounts []}} statements)
          report2 (make-tax-report {} statements)
          report3 (make-tax-report {:filters-map {:credit-accounts ["456"]}} statements)]
      (are [x y] (= x y)
        3 (count (:statements report1))
        3 (count (:statements report2))
        1 (count (:statements report3))

        (get statements-map "2") (-> (first (:statements report3))
                                     (dissoc :inferred-currency-rate))

        150.0 (get-in report1 [:taxable-amount :USD])
        150.0 (get-in report2 [:taxable-amount :USD])
        100.0 (get-in report3 [:taxable-amount :USD])
        (+ 40955.23 (* 150 usd-uah)) (get-in report1 [:taxable-amount :UAH])
        (+ 40955.23 (* 150 usd-uah)) (get-in report2 [:taxable-amount :UAH])
        (* 100 usd-uah) (get-in report3 [:taxable-amount :UAH])))))
