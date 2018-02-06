(ns privat24-clj.session
  (:require [privat24-clj.api :as api]
            [clojure.tools.logging :as log]))

(def ^:private b-session-and-err [nil nil])

(defn- bind-error
  [f [val err]] (if err [val err] (f val)))

(defn- handle-otp-dev-step
  [b-session ask-otp-dev-fn]
  (if (seq (.otp-devices b-session))
    (let [devices (.otp-devices b-session)
          otp-index (int (ask-otp-dev-fn devices))
          device (nth devices otp-index)]
      (api/select-otp-device b-session (:id device)))
    [b-session nil]))

(defn- handle-otp-step
  [b-session ask-otp-fn]
  (if (.business-role? b-session)
    [b-session nil]
    (let [otp (ask-otp-fn)]
      (api/check-otp b-session otp))))

(defn authenticate-b-session
  ([credentials ask-otp-fn ask-otp-dev-fn]
   {:pre [(vector? credentials)
          (= 4 (count credentials))
          (every? fn? [ask-otp-fn ask-otp-dev-fn])]}

   (log/info "Performing authentication")
   (let [[client-id client-secret pb24b-login pb24b-password] credentials]
     (->> (api/create-session client-id client-secret)
          (bind-error #(api/create-business-session % pb24b-login pb24b-password))
          (bind-error #(handle-otp-dev-step % ask-otp-dev-fn))
          (bind-error #(handle-otp-step % ask-otp-fn))))))

(defn refresh-b-session
  [& auth-args]
  (locking b-session-and-err
    (let [[b-session err] b-session-and-err]
      (cond
        err [nil err]
        (or (not b-session) (.expired? b-session)) (let [result (apply authenticate-b-session
                                                                      auth-args)]
                                                    (def b-session-and-err result)
                                                    result)
        :else b-session-and-err))))
