(ns privat24-clj.config)

(defn- env [key] (System/getenv (str key)))

(defn- from-env []
  {:client-id       (env "PRIVAT24_CLIENT_ID")
   :client-secret   (env "PRIVAT24_CLIENT_SECRET")
   :pb24b-login     (env "PRIVAT24_BUSINESS_LOGIN")
   :pb24b-password  (env "PRIVAT24_BUSINESS_PASSWORD")})

(defn credentials []
  (from-env))
