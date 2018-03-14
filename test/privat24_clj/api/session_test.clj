(ns privat24-clj.api.session-test
  (:require [clojure.test :refer :all]
            [privat24-clj.api :as api]
            [privat24-clj.api.session :refer :all]))

(defn auth-session-tests []
  {"with error on first step"
   [{:error "Some error"}
    (fn [path data]
      (case path
        "/auth/createSession" {:status 200
                               :error "Some error"}))]

   "with error on second step"
   [{:error "Some error2"}
    (fn [path data]
      (case path
        "/auth/createSession" {:data {:id "token1"}
                               :status 200}
        "/p24BusinessAuth/createSession" {:error "Some error2"
                                          :status 200}))]

   "without otp"
   [{:token "token2"}
    (fn [path data]
      (case path
        "/auth/createSession" {:data {:id "token1"}
                               :status 200}
        "/p24BusinessAuth/createSession" {:data
                                          {:id "token2"
                                           :roles [api/business-role-str]}
                                          :status 200}))]

   "without otp dev"
   [{:token "token3"}
    (fn [path data]
      (case path
        "/auth/createSession" {:data {:id "token1"}
                               :status 200}
        "/p24BusinessAuth/createSession" {:data
                                          {:id "token2"}
                                          :status 200}
        "/p24BusinessAuth/checkOtp" {:data
                                     {:id "token3"
                                      :roles [api/business-role-str]}
                                     :status 200}))]

   "with otp dev"
   [{:token "token3"}
    (fn [path data]
      (case path
        "/auth/createSession" {:data {:id "token1"}
                               :status 200}
        "/p24BusinessAuth/createSession" {:data
                                          {:id "token2"
                                           :message [{:id "1" :number "123"}]}
                                          :status 200}
        "/p24BusinessAuth/sendOtp" {:status 200}
        "/p24BusinessAuth/checkOtp" {:data
                                     {:id "token3"
                                      :roles [api/business-role-str]}
                                     :status 200}))]})

(deftest authenticate-b-session-test
  (let [credentials {:client-id "id" :client-secret "client-secret"
                     :pb24b-login "login" :pb24b-password "password"}
        token1 "token"
        token2 "token2"
        token3 "token3"
        ask-otp-fn (constantly "123")
        ask-otp-dev-fn (constantly "1")
        otp-devices [{:id "1" :number "123"}]
        auth #(authenticate-b-session credentials ask-otp-fn ask-otp-dev-fn)]
    (doseq [[test-name [expected-result request-stub]] (auth-session-tests)]
      (testing test-name
        (with-redefs [api/post-request request-stub]
          (is (= expected-result (auth))))))))
