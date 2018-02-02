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
  (otp-numbers [this]))

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
  (otp-numbers [_] (let [msg (:message data)]
                     (if (coll? msg) msg []))))

(defn- make-uri [path] (str base-uri path))

(defn- with-default-headers
  ([] default-headers)
  ([& h] (apply merge default-headers h)))

(defn- with-error
  ([response] (with-error #(.error %) response))
  ([error-fn response]
   [response (error-fn response)]))

(defn post
  ([path data] (post path data default-headers))
  ([path data headers]
   
   (let [uri (make-uri path)
         headers (with-default-headers headers)
         body (json/encode data)]
     (log/info "POST" uri body headers)
     (PB24Response.
       (http/post
         uri
         (merge
           headers
           {:body body}
           {:throw-exceptions false}))))))

(defn create-session
  "Sends request for getting basic client session
   returns [session err]"
  ([] (create-session client-id client-secret))
  ([client-id# client-secret#]
   (let [path "/auth/createSession"
         data {:clientId client-id#
               :clientSecret client-secret#}
         response (post path data)]
     (if-let [err (.error response)]
       [response err]
       [(PB24Session. (.data response)) nil]))))

(defn validate-session
  [session]
  (let [path "/auth/validatesession"
        data {:sessionId (.token session)}
        response (post path data)]
    (.success? response)))

(defn create-business-session
  "Sens request for getting business client session
  returns [session err]"
  ([session] (create-business-session pb24b-login pb24b-password))
  ([session pb24b-login pb24b-password]
   (let [path "/p24BusinessAuth/createSession"
         data {:sessionId (.token session)
               :login pb24b-login
               :password pb24b-password}
         response (post path data)]
     (if-let [err (.error response)]
       [response err]
       [(PB24Session. (.data response)) nil]))))

(defn select-otp-device
  "Sends request for selecting device which will receive OTP"
  [b-session device-id]
  (let [path "/p24BusinessAuth/sendOtp"
        data {:sessionId (.token b-session)
              :otpDev device-id}
        response (post path data)]
    (if-let [err (.error response)]
      [response err]
      [(->> response .data :message) nil])))

(defn check-otp
  "Sends request for checking OTP"
  [b-session otp]
  (let [path "/p24BusinessAuth/checkOtp"
        data {:sessionId (.token b-session)
              :otp otp}
        response (post path data)
        new-session (PB24Session. (.data response))
        err (.error response)]
    (cond
      err [response err]
      (->> new-session business-session? not) [response (->> response .data :message)]
      :else [new-session nil])))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
