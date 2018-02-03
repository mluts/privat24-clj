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

(def ^:const ttl-delta 30) ; 30 seconds

(def b-session-and-err (agent [nil nil]))

(defn- timestamp [] (int (/ (System/currentTimeMillis) 1000)))

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

(defn- bind-error
  [f [val err]]
  (if err
    [val err]
    (f val)))

(defn- make-uri [path] (str base-uri path))

(defn- with-default-headers
  ([] default-headers)
  ([& h] (apply merge default-headers h)))

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
  ([] (create-session client-id client-secret))
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

(defn handle-otp-dev-step
  [b-session ask-otp-dev-fn]
  (if (seq (.otp-devices b-session))
    (let [devices (.otp-devices b-session)
          otp-index (int (ask-otp-dev-fn devices))
          device (nth devices otp-index)]
      (select-otp-device b-session (:id device)))
    [b-session nil]))

(defn handle-otp-step
  [b-session ask-otp-fn]
  (if (.business-role? b-session)
    [b-session nil]
    (let [otp (ask-otp-fn)]
      (check-otp b-session otp))))

(defn authenticate-full
  ([] (authenticate-full default-ask-otp default-ask-otp-dev))
  ([ask-otp-fn ask-otp-dev-fn]
   (log/info "Performing authentication")
   (->> (create-session)
       (bind-error create-business-session)
       (bind-error #(handle-otp-dev-step % ask-otp-dev-fn))
       (bind-error #(handle-otp-step % ask-otp-fn)))))

(defn authenticated-request
  [req & args]
  (future (await b-session-and-err)
          (let [[b-session err] @b-session-and-err]
            (cond
              err [nil err]
              (or (not b-session)
                  (expired? b-session)) (do
                                          (send b-session-and-err (fn [_] (authenticate-full)))
                                          @(apply authenticated-request req args))
              :else (apply req b-session args)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
