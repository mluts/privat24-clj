(ns privat24-clj.cli
  (:require [privat24-clj.session :as session]
            [privat24-clj.api :as api]
            [privat24-clj.creds :as creds]
            [privat24-clj.util :refer [bind-error]]))

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
       (map-indexed (fn [i dev] (str i ": " (.number dev))))
       (map println)
       doall))

(defn get-creds [] (creds/from-env))

(defn auth-request [req & args]
  (let [req-fn #(apply req % args)]
    (as-> (session/authenticate-b-session (get-creds)
                                          default-ask-otp
                                          default-ask-otp-dev) $
      (bind-error req-fn $))))

(defn check-auth []
  (auth-request api/validate-session))

(defn business-statements [date-start date-end]
  (auth-request api/business-statements date-start date-end))
