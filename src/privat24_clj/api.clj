(ns privat24-clj.api
  (:require [clj-http.client :as http]
            [cheshire.core :as json]
            [taoensso.timbre :as log]
            [privat24-clj.util :refer [<-json ->json]]
            [slingshot.slingshot :refer [try+]]))

(def base-uri "https://link.privatbank.ua/api")

(def default-headers {:content-type "application/json"
                      :accept "application/json"})

(def ttl-delta 30) ; 30 seconds

(def business-role-str "ROLE_P24_BUSINESS")

(defn- timestamp [] (int (/ (System/currentTimeMillis) 1000)))

(defn- make-uri [path] (str base-uri path))

(defn- with-default-headers
  ([] default-headers)
  ([& h] (apply merge default-headers h)))

(defn parse-response [{:keys [status body]}]
  (let [status (:status raw)
        data (try (<-json body)
                  (catch com.fasterxml.jackson.core.JsonParseException _ {:error "Failed to parse the response"}))
        error (:error data)]
    {:raw raw
     :data data
     :error (cond
              error error
              (not (<= 200 status 299)) (str "HTTP Status: " status)
              :else nil)}))

(defn parse-session [response]
  (let [data (:data response)
        roles (get data :roles [])
        message (:message data)
        parse-device (fn [data] (select-keys data [:id :number]))
        otp-devices (if (sequential? message) (map parse-device message) [])
        session {:expires_in (get data :expiresIn 0)
                 :token (:id data)
                 :message (when (string? message) message)
                 :otp-devices otp-devices
                 :is-business-role (some (partial = business-role-str) roles)}]
    (assoc response :session session)))

(defn session-expired? [session]
  (<= (get session :expires_in 0) (timestamp)))

(defn get-request
  ([path params token]
   (let [uri (make-uri path)
         headers (merge default-headers
                        {:authorization (str "Token " token)})]
     (log/info "GET" uri headers params)
     (->> (http/get uri {:query-params params
                         :headers headers})
          parse-response))))

(defn post-request
  ([path data] (post-request path data {}))
  ([path data headers]
   (let [uri (make-uri path)
         headers (merge default-headers headers)]
     (log/info "POST" uri headers)
     (->> (http/post uri {:body (->json data)
                          :headers headers
                          :throw-exceptions false})
          parse-response))))

(defn create-session
  "Sends request for getting basic client session
  see https://link.privatbank.ua/console/wiki/client_auth"
  ([client-id client-secret]
   (->> (post-request "/auth/createSession" {:clientId client-id
                                             :clientSecret client-secret})
        parse-session)))

(defn validate-session [token]
  (post-request "/auth/validateSession" {:sessionId token}))

(defn create-business-session
  "Sens request for getting business client session"
  ([token pb24b-login pb24b-password]
   (->> (post-request "/p24BusinessAuth/createSession" {:sessionId token
                                                        :login pb24b-login
                                                        :password pb24b-password})
        parse-session)))

(defn select-otp-device
  "Sends request for selecting device which will receive OTP"
  [token device-id]
  (post-request "/p24BusinessAuth/sendOtp" {:sessionId token
                                            :otpDev device-id}))

(defn check-otp
  "Sends request for checking OTP"
  [token otp]
  (->> (post-request "/p24BusinessAuth/checkOtp" {:sessionId token
                                                  :otp otp})
       parse-session))

(defn business-statements
  "Requests statements for all accounts from/to given dates"
  [token start-date end-date]
  (get-request "/p24b/statements/" {:stdate start-date
                                    :endate end-date
                                    :showInf ""} token))
