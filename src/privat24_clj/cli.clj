(ns privat24-clj.cli
  (:require [privat24-clj.api.session :as session]
            [privat24-clj.api :as api]
            [privat24-clj.config :as config]
            [privat24-clj.api.statement :as st]
            [privat24-clj.taxes :as tax]))

(defn default-ask-otp
  []
  (println "Please enter OTP: ")
  (->> *in*
       clojure.java.io/reader
       line-seq
       first))

(defn default-ask-otp-dev
  [devs]
  (println "Select otp device: ")
  (->> devs
       (map-indexed (fn [i dev] (str i ": " dev)))
       (map println)
       doall))

(defn get-token []
  (session/authenticate-b-session (config/credentials) default-ask-otp default-ask-otp-dev))

(defn auth-request [req & args]
  (let [req-fn #(apply req % args)
        {:keys [error token] :as result} (get-token)]
    (if error
      result
      (apply req token args))))

(defn check-auth []
  (auth-request api/validate-session))

(defn business-statements [date-start date-end]
  (auth-request api/business-statements date-start date-end))

(defn tax-report [quarter name filters-m]
  (let [[date-start date-end] quarter
        {:keys [error] :as response} (auth-request api/business-statements date-start date-end)]
    (if error
      response
      (->> (:data response)
           (map st/parse-row)
           (tax/make-tax-report {:filters-map filters-m
                                 :date-start date-start
                                 :date-end date-end
                                 :name name})))))

(defn prev-quarter-tax-report [filters-m]
  (tax-report (tax/prev-quarter-range) "previous quarter report" filters-m))

(defn current-quarter-tax-report [filters-m]
  (tax-report (tax/current-quarter-range) "current quarter report" filters-m))
