(ns privat24-clj.session
  (:require [privat24-clj.api :as api]
            [privat24-clj.util :refer [bind-error]]
            [taoensso.timbre :as log])
  (:import [privat24_clj.api PB24Session]))

(defn- get-otp-device-id [devices ask-otp-dev-fn]
  (let [otp-index (ask-otp-dev-fn devices)
        device (nth devices otp-index)]
    (:id device)))

(defn- handle-otp-dev-step [b-session ask-otp-dev-fn]
  {:pre [(instance? PB24Session b-session)
         (fn? ask-otp-dev-fn)]}
  (log/info "Handling otp dev step")
  (let [devices (.otp-devices b-session)]
    (if (seq devices)
     (api/select-otp-device b-session
                            (get-otp-device-id devices ask-otp-dev-fn))
     [b-session nil])))

(defn- handle-otp-step [b-session ask-otp-fn]
  (log/info "Handling otp step")
  (if (.business-role? b-session)
    [b-session nil]
    (let [otp (ask-otp-fn)]
      (api/check-otp b-session otp))))

(defn authenticate-b-session ([credentials ask-otp-fn ask-otp-dev-fn]
   {:pre [(map? credentials)
          (= 4 (count credentials))
          (every? fn? [ask-otp-fn ask-otp-dev-fn])]}

   (log/info "Performing authentication")
   (let [{:keys [client-id client-secret pb24b-login pb24b-password]} credentials]
     (as-> (api/create-session client-id client-secret) $
       (bind-error api/create-business-session $ pb24b-login pb24b-password)
       (bind-error handle-otp-dev-step $ ask-otp-dev-fn)
       (bind-error handle-otp-step $ ask-otp-fn)))))

(defn refresh-b-session [b-session & auth-args]
  {:pre [(instance? PB24Session b-session)]}
  (if (.expired? b-session)
    (apply authenticate-b-session auth-args)
    [b-session nil]))
