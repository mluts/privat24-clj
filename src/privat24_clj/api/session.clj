(ns privat24-clj.api.session
  (:require [privat24-clj.api :as api]
            [taoensso.timbre :as log]))

(defn get-otp-device-id [devices ask-otp-dev-fn]
  {:pre [(sequential? devices)
         (every? #(and (contains? % :id)
                       (contains? % :number))
                 devices)]}
  (as-> (ask-otp-dev-fn devices) $
    (nth devices $ {})
    (:id $)))

(defn handle-business-session-step [pb24b-login pb24b-password response]
  {:pre [(string? pb24b-login)
         (string? pb24b-password)
         (map? response)]}
  (log/info "Upgrading session step")
  (api/create-business-session (get-in response [:session :token])
                               pb24b-login
                               pb24b-password))

(defn handle-otp-dev-step [ask-otp-dev-fn response]
  {:pre [(fn? ask-otp-dev-fn)]}
  (log/info "Handling otp dev step")
  (let [devices (get-in response [:session :otp-devices])]
    (if (and (sequential? devices) (not-empty devices))
      (api/select-otp-device (get-in response [:session :token])
                             (get-otp-device-id devices ask-otp-dev-fn))
      response)))

(defn handle-otp-step [ask-otp-fn response]
  {:pre [(map? response) (fn? ask-otp-fn)]}

  (log/info "Handling otp step")
  (if (get-in response [:session :is-business-role])
    response
    (let [otp (ask-otp-fn)
          token (get-in response [:session :token])
          response (api/check-otp token otp)]
      (cond
        (:error response) response
        (not (get-in response [:session :is-business-role])) (assoc response
                                                                    :error
                                                                    "Failed to authenticate business role")
        :else response))))

(defn authenticate-b-session
  "Returns parsed response"
  [credentials ask-otp-fn ask-otp-dev-fn]
  {:pre [(map? credentials)
         (= 4 (count credentials))
         (every? fn? [ask-otp-fn ask-otp-dev-fn])]}

  (log/info "Performing authentication")
  (let [{:keys [client-id client-secret pb24b-login pb24b-password]} credentials
        call-step-fn (fn [response step-fn]
                       (if (:error response) response (step-fn response)))]
    (reduce call-step-fn
            (api/create-session client-id client-secret)
            [(partial handle-business-session-step pb24b-login pb24b-password)
             (partial handle-otp-dev-step ask-otp-dev-fn)
             (partial handle-otp-step ask-otp-fn)])))
