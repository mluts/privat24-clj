(ns privat24-clj.session
  (:require [privat24-clj.api :as api]
            [clojure.tools.logging :as log]))

(def ^:private b-session-and-err (agent [nil nil]))

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
  (send
    b-session-and-err
    (fn
      [[b-session err old-value :as all]]
      (cond
        err old-value
        (or (not b-session)
            (.expired? b-session)) (try (apply authenticate-b-session auth-args)
                                        (catch Exception e [nil (.getMessage e)]))
        :else old-value)))
  (await b-session-and-err)
  @b-session-and-err)
