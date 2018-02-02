(ns privat24-business.core
  (:gen-class)
  (:require [clj-http.client :as http]
            [cheshire.core :as json]
            [clojure.tools.logging :as log]
            [slingshot.slingshot :refer [try+]]))

(def client-id (System/getenv "PRIVAT24_CLIENT_ID"))
(def client-secret (System/getenv "PRIVAT24_CLIENT_SECRET"))

(def pb24b-login (System/getenv "PRIVAT24_BUSINESS_LOGIN"))
(def pb24b-password (System/getenv "PRIVAT24_BUSINESS_PASSWORD"))

(def base-uri "https://link.privatbank.ua/api")
(def default-headers {:content-type "application/json"
                      :accept "application/json"})

(defprotocol Response
  (success? [_])
  (error [_])
  (data [_]))

(defprotocol Session
  (expired? [_])
  (token [_]))

(defprotocol SessionRole
  (business-role? [this])
  (message [this])
  (otp-devices [this]))

(defprotocol OTPDevice
  (id [this])
  (number [this]))

(deftype PB24OTPDevice
  [raw]
  OTPDevice
  (id [_] (:id raw))
  (number [_] (:number raw)))

(deftype PB24Response
  [raw]
  Response
  (success? [_] (<= 200 (:status raw) 299))
  (error [this] (when-not (.success? this)
                  (-> this (.data) :error)))
  (data [this] (let [body (:body raw)]
                 (try+ (json/decode body keyword)
                     (catch com.fasterxml.jackson.core.JsonParseException _ {})))))

(deftype PB24Session [data]
  Session
  (expired? [_] (int (<= (:expiresIn data)
                         (System/currentTimeMillis))))
  (token [_] (:id data))

  SessionRole
  (business-role? [_] (->> data
                              :roles
                              (some #(= "ROLE_P24_BUSINESS" %))))
  (message [_] (let [msg (:message data)]
                 (when-not (coll? msg) msg)))
  (otp-devices [_] (let [msg (:message data)]
                     (if (coll? msg) (map #(PB24OTPDevice. %) msg) []))))

(defn- make-uri [path] (str base-uri path))

(defn- with-default-headers
  ([] default-headers)
  ([& h] (apply merge default-headers h)))

(defn- with-error
  ([response] (with-error #(.error %) response))
  ([error-fn response]
   [response (error-fn response)]))

(defn get-request
  ([path params session]
   (let [uri (make-uri path)
         headers (with-default-headers
                   {:authorization (str "Token " (.token session))})]
     (log/info "GET" uri headers params)
     (PB24Response. (http/get uri {:query-params params :headers headers})))))

(defn post-request
  ([path data] (post-request path data default-headers))
  ([path data headers]
   
   (let [uri (make-uri path)
         headers (with-default-headers headers)
         body (json/encode data)]
     (log/info "POST" uri body headers)
     (PB24Response.
       (http/post
         uri
         {:body body
          :headers headers
          :throw-exceptions false})))))

(defn create-session
  "Sends request for getting basic client session
   returns [session err]"
  ([] (create-session client-id client-secret))
  ([client-id# client-secret#]
   (let [path "/auth/createSession"
         data {:clientId client-id#
               :clientSecret client-secret#}
         response (post-request path data)]
     (if-let [err (.error response)]
       [response err]
       [(PB24Session. (.data response)) nil]))))

(defn validate-session
  [session]
  (let [path "/auth/validatesession"
        data {:sessionId (.token session)}
        response (post-request path data)]
    (.success? response)))

(defn create-business-session
  "Sens request for getting business client session
  returns [session err]"
  ([session] (create-business-session session pb24b-login pb24b-password))
  ([session pb24b-login pb24b-password]
   (let [path "/p24BusinessAuth/createSession"
         data {:sessionId (.token session)
               :login pb24b-login
               :password pb24b-password}
         response (post-request path data)]
     (if-let [err (.error response)]
       [response err]
       [(PB24Session. (.data response)) nil]))))

(defn select-otp-device
  "Sends request for selecting device which will receive OTP"
  [b-session device-id]
  (let [path "/p24BusinessAuth/sendOtp"
        data {:sessionId (.token b-session)
              :otpDev device-id}
        response (post-request path data)]
    (if-let [err (.error response)]
      [response err]
      [(->> response .data :message) nil])))

(defn check-otp
  "Sends request for checking OTP"
  [b-session otp]
  (let [path "/p24BusinessAuth/checkOtp"
        data {:sessionId (.token b-session)
              :otp otp}
        response (post-request path data)
        new-session (PB24Session. (.data response))
        err (.error response)]
    (cond
      err [response err]
      (->> new-session .business-role? not) [response (->> response .data :message)]
      :else [new-session nil])))

(defn business-statements
  "Requests statements for all accounts from/to given dates"
  [b-session start-date end-date]
  (let [path  "/p24b/statements/"
        params {:stdate start-date
                :endate end-date
                :showInf ""}
        response (get-request path params b-session)]
    (if-let [err (.error response)]
      [response err]
      [(.data response) nil])))

(defn- perform-otp-auth
  [b-session
   ask-otp-fn
   ask-otp-dev-fn]
  (cond
    (.business-role? b-session) [b-session nil]
    (seq (.otp-devices b-session)) (let [devices (.otp-devices b-session)
                                         otp-index (int (ask-otp-dev-fn devices))
                                         device (nth devices otp-index)]
                                     (let [[msg err] (select-otp-device b-session (:id device))]
                                       (if err
                                         [msg err]
                                         (let [otp (ask-otp-fn)]
                                           (check-otp b-session otp)))))
    :else (let [otp (ask-otp-fn)]
            (check-otp b-session otp))))

(defn default-ask-otp
  []
  (println "Please enter OTP: ")
  (->> *in*
       clojure.java.io/reader
       line-seq
       first))

(defn default-ask-otp-dev
  [devs]
  (println "Select otp device:")
  (doall (map-indexed #(println (str %1 ": " (.number %2)))
                      devs)))

(defn authenticate-full
  ([] (authenticate-full default-ask-otp default-ask-otp-dev))
  ([ask-otp-fn
    ask-otp-dev-fn]
   (let [[session err] (create-session)]
     (if err
       [nil err]
       (let [[b-session err] (create-business-session session)]
         (if err
           [nil err]
           (perform-otp-auth b-session ask-otp-fn ask-otp-dev-fn)))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
