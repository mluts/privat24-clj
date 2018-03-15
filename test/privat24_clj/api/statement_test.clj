(ns privat24-clj.api.statement-test
  (:require [privat24-clj.api.statement :refer :all]
            [clojure.test :refer :all]
            [privat24-clj.test.util :refer [statements-example]]))

(deftest parse-row-test
  (let [m (into {} (map (juxt :id identity)) (statements-example))
        {statement1 "3"} m]
    (is (= 28.47
           (get-in statement1 [:detected-currency-rate :USD :UAH])))))
