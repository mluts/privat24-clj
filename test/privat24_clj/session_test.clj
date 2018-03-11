(ns privat24-clj.session-test
  (:require [clojure.test :refer :all]
            [privat24-clj.api :as api]
            [privat24-clj.session :refer :all]))

(deftest authenticate-b-session-test
  (let [credentials {:client-id "id" :client-secret "client-secret"
                     :pb24b-login "login" :pb24b-password "password"}
        token1 "token"
        token2 "token2"
        token3 "token3"
        ask-otp-fn (constantly "123")
        ask-otp-dev-fn (constantly 0)
        otp-devices [{:id "1" :number "123"}]]
    (testing "it performs all steps successfully"
      (with-redefs [api/post-request (fn [path data]
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
                                                                      :status 200}))]
        (is (= token3
               (get-in (authenticate-b-session credentials ask-otp-fn ask-otp-dev-fn)
                       [:session :token])))))
    (testing "it performs without otp dev step"
      (with-redefs [api/post-request (fn [path data]
                                       (case path
                                         "/auth/createSession" {:data {:id token1}
                                                                :status 200}
                                         "/p24BusinessAuth/createSession" {:data
                                                                           {:id token2}
                                                                           :status 200}
                                         "/p24BusinessAuth/checkOtp" {:data
                                                                      {:id token3
                                                                       :roles [api/business-role-str]}
                                                                      :status 200}))]
        (is (= token3
               (get-in (authenticate-b-session credentials ask-otp-fn ask-otp-dev-fn)
                       [:session :token])))))))
