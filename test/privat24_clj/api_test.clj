(ns privat24-clj.api-test
  (:require [privat24-clj.api :refer :all]
            [clj-http.client :as http]
            [privat24-clj.util :refer [<-json ->json]]
            [clojure.test :refer :all]))

(defn stub-post [path options response]
  (let [k [:body :headers]]
    (fn [uri req-options]
      (assert (and (= (select-keys options k)
                      (select-keys req-options k))
                   (= (str base-uri path) uri)))
      response)))

(deftest create-session-test
  (let [client-id "client-id"
        client-secret "client-secret"
        token "token"
        request-body (->json {:clientId client-id
                              :clientSecret client-secret})]
    (testing "it makes correct post request"
      (with-redefs [http/post (stub-post "/auth/createSession"
                                         {:body request-body
                                          :headers default-headers}
                                         {:status 200
                                          :body (->json {:id token})})]
        (is (= token
               (get-in (create-session client-id client-secret) [:session :token])))))))

(deftest create-business-session-test
  (let [login "login"
        password "password"
        token "token"
        token2 "token2"
        otp-devices [{:id "1" :number "123"}
                     {:id "2" :number "456"}]
        request-body (->json {:sessionId token
                              :login login
                              :password password})
        response-body (->json {:id token2
                               :message otp-devices})]
    (with-redefs [http/post (stub-post "/p24BusinessAuth/createSession"
                                       {:body request-body
                                        :headers default-headers}
                                       {:status 200
                                        :body response-body})]
      (testing "otp-devices parsed ok"
        (is (= otp-devices
               (get-in (create-business-session token login password)
                       [:session :otp-devices])))))))
