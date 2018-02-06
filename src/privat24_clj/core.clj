(ns privat24-clj.core
  (:gen-class)
  (:require [privat24-clj.session :as session]
            [privat24-clj.api :as api]))

(def client-id (System/getenv "PRIVAT24_CLIENT_ID"))
(def client-secret (System/getenv "PRIVAT24_CLIENT_SECRET"))

(def pb24b-login (System/getenv "PRIVAT24_BUSINESS_LOGIN"))
(def pb24b-password (System/getenv "PRIVAT24_BUSINESS_PASSWORD"))

(defn- bind-error
  [f [val err]]
  (if err [val err] (f val)))

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

(defmulti authenticated-request
  (fn [arg & args]
    (if (fn? arg)
      :default-credentials
      :custom-credentials)))

(defmethod authenticated-request :default-credentials
  ([req & args] (apply authenticated-request
                       [[client-id client-secret
                         pb24b-login pb24b-password]
                        default-ask-otp
                        default-ask-otp-dev]
                       req
                       args)))

(defmethod authenticated-request :custom-credentials
  ([auth-args req & args]
   (->> (apply session/refresh-b-session auth-args)
        (bind-error #(apply req % args)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
