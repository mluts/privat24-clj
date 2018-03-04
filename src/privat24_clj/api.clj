(ns privat24-clj.api
  (:require [clj-http.client :as http]
            [cheshire.core :as json]
            [taoensso.timbre :as log]
            [privat24-clj.util :refer [<-json ->json]]
            [slingshot.slingshot :refer [try+]]))

(def base-uri "https://link.privatbank.ua/api")

(def default-headers {:content-type "application/json"
                      :accept "application/json"})

(def ^:const ttl-delta 30) ; 30 seconds

(defn- timestamp [] (int (/ (System/currentTimeMillis) 1000)))

(defprotocol Response
  (status [_])
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

(defrecord PB24OTPDevice [raw]
  OTPDevice
  (id [_] (:id raw))
  (number [_] (:number raw)))

(defrecord PB24Response [raw]
  Response
  (status [_] (:status raw))
  (success? [this] (<= 200 (.status this) 299))
  (error [this] (when-not (.success? this)
                  (-> this (.data) :error)))
  (data [this] (let [body (:body raw)]
                 (try+ (<-json body)
                     (catch com.fasterxml.jackson.core.JsonParseException _ {})))))

(defrecord PB24Session [data]
  Session
  (expired? [_] (<= (:expiresIn data)
                    (- (timestamp) ttl-delta)))
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

(defn- get-request
  ([path params session]
   (let [uri (make-uri path)
         headers (with-default-headers
                   {:authorization (str "Token " (.token session))})]
     (log/info "GET" uri headers params)
     (PB24Response. (http/get uri {:query-params params :headers headers})))))

(defn- post-request
  ([path data] (post-request path data default-headers))
  ([path data headers]
   
   (let [uri (make-uri path)
         headers (with-default-headers headers)
         body (->json data)]
     (log/info "POST" uri headers)
     (PB24Response.
       (http/post
         uri
         {:body body
          :headers headers
          :throw-exceptions false})))))

(defn create-session
  "Sends request for getting basic client session
   returns [session err]"
  ([client-id client-secret]
   (let [path "/auth/createSession"
         data {:clientId client-id
               :clientSecret client-secret}
         response (post-request path data)]
     (if-let [err (.error response)]
       [response err]
       [(PB24Session. (.data response)) nil]))))

(defn validate-session
  [session]
  (let [path "/auth/validateSession"
        data {:sessionId (.token session)}
        response (post-request path data)]
    (if (.success? response)
      [true nil]
      [false (str (.status response) (.data response))])))

(defn create-business-session
  "Sens request for getting business client session
  returns [session err]"
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
