(ns privat24-clj.api.session
  (:require [privat24-clj.api :as api]
            [taoensso.timbre :as log]))

(defn create-session
  "Sends request to create basic session"
  [state client-id client-secret]
  {:pre [(map? state) (string? client-id) (string? client-secret)]}
  (let [{:keys [error]
         {:keys [token]} :session} (api/create-session client-id client-secret)]
    (if error {:error error} {:basic-token token})))

(defn create-business-session
  "Sends request to upgrade basic session token to business-session token"
  [{:keys [error basic-token] :as state} pb24b-login pb24b-password]
  {:pre [(map? state) (string? pb24b-login) (string? pb24b-password)]}
  (if error
    state
    (let [{:keys [error]
           {:keys [token otp-devices is-business-role]} :session} (api/create-business-session basic-token
                                                                                               pb24b-login
                                                                                               pb24b-password)]
      (if error
        {:error error}
        (assoc state
               :otp-devices otp-devices
               :business-token token
               :is-business-role is-business-role)))))

(defn select-otp-dev
  "Asks for otp device id and sends request to choose otp device if necessary"
  [{:keys [error business-token otp-devices is-business-role] :as state} ask-otp-dev-fn]
  {:pre [(fn? ask-otp-dev-fn)]}
  (if (or error (empty? otp-devices) is-business-role)
    state
    (let [otp-dev-id (ask-otp-dev-fn otp-devices)
          {:keys [error]} (api/select-otp-device business-token otp-dev-id)]
      (assoc state :error error))))

(defn send-otp
  "Asks for otp using (ask-otp-fn) and sends OTP to api"
  [{:keys [error business-token is-business-role] :as state} ask-otp-fn]
  {:pre [(fn? ask-otp-fn)]}
  (cond
    error state
    is-business-role {:token business-token}
    :else (let [otp (ask-otp-fn)
                {:keys [error]
                 {:keys [token is-business-role]} :session} (api/check-otp business-token otp)]
            (cond
              error (assoc state :error error)
              (not is-business-role) (assoc state :error "Failed to authenticate business role")
              :else {:token token}))))

(defn authenticate-b-session
  "Returns parsed response"
  [{:keys [client-id client-secret pb24b-login pb24b-password]} ask-otp-fn ask-otp-dev-fn]
  (-> (create-session {} client-id client-secret)
      (create-business-session pb24b-login pb24b-password)
      (select-otp-dev ask-otp-dev-fn)
      (send-otp ask-otp-fn)))
