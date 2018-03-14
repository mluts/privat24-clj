(ns privat24-clj.api.session-test
  (:require [clojure.test :refer :all]
            [privat24-clj.api :as api]
            [privat24-clj.api.session :refer :all]))

(defn with-otp-dev-mock [token1 token2 token3 otp-devices]
  (fn [path data]
    (case path
      "/auth/createSession" {:data {:id token1}
                             :status 200}
      "/p24BusinessAuth/createSession" {:data
                                        {:id token2
                                         :message otp-devices}
                                        :status 200}
      "/p24BusinessAuth/sendOtp" {:status 200}
      "/p24BusinessAuth/checkOtp" {:data
                                   {:id token3
                                    :roles [api/business-role-str]}
                                   :status 200})))

(defn without-otp-dev-mock [token1 token2 token3]
  (fn [path data]
    (case path
      "/auth/createSession" {:data {:id token1}
                             :status 200}
      "/p24BusinessAuth/createSession" {:data
                                        {:id token2}
                                        :status 200}
      "/p24BusinessAuth/checkOtp" {:data
                                   {:id token3
                                    :roles [api/business-role-str]}
                                   :status 200})))

(defn without-otp-mock [token1 token2]
  (fn [path data]
    (case path
      "/auth/createSession" {:data {:id token1}
                             :status 200}
      "/p24BusinessAuth/createSession" {:data
                                        {:id token2
                                         :roles [api/business-role-str]}
                                        :status 200})))

(deftest authenticate-b-session-test
  (let [credentials {:client-id "id" :client-secret "client-secret"
                     :pb24b-login "login" :pb24b-password "password"}
        token1 "token"
        token2 "token2"
        token3 "token3"
        ask-otp-fn (constantly "123")
        ask-otp-dev-fn (constantly 0)
        otp-devices [{:id "1" :number "123"}]
        auth #(authenticate-b-session credentials ask-otp-fn ask-otp-dev-fn)
        with-otp-dev-res (with-redefs [api/post-request (with-otp-dev-mock token1 token2 token3 otp-devices)] (auth))
        without-otp-dev-res (with-redefs [api/post-request (without-otp-dev-mock token1 token2 token3)] (auth))
        without-otp-res (with-redefs [api/post-request (without-otp-mock token1 token2)] (auth))]

    (testing "it performs with otp dev step"
      (are [x y] (= x y)
        nil (:error with-otp-dev-res)
        token3 (:token with-otp-dev-res)))

    (testing "it performs without otp dev step"
      (are [x y] (= x y)
        nil (:error without-otp-dev-res)
        token3 (:token without-otp-dev-res)))

    (testing "it performs without otp completely"
      (are [x y] (= x y)
        nil (:error without-otp-res)
        token2 (:token without-otp-res)))))
