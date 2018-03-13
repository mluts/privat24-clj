(ns privat24-clj.core
  (:require [privat24-clj.cli :as cli]
            [privat24-clj.util :refer [->json]]
            [clojure.tools.cli :as opts]
            [clojure.string :as string]
            [taoensso.timbre :as timbre])
  (:gen-class))

(defn check-auth [opts]
  (let [{:keys [error]} (cli/check-auth)]
    (if error
      (println "ERR: " error)
      (println "OK"))))

(defn business-statements [opts date-start date-end]
  (let [{:keys [error data]} (cli/business-statements date-start date-end)]
    (if error
      (println "ERR: " error)
      (println (->json data {:pretty true})))))

(defn get-token [opts]
  (-> (cli/get-token)
      (get-in [:session :token])
      str
      println))

(defn tax-report [opts]
  (let [{:keys [error] :as data} (cli/current-quarter-tax-report (select-keys opts [:credit-accounts]))]
    (if error
      (println "ERR: " error)
      (println (->json data {:pretty true})))))

(def cli-options
  [["-v" "--verbose"]
   ["-C" "--credit-accounts ACCOUNT[,ACCOUNT...]" "Credit account numbers separated by comma"
    :parse-fn #(string/split % #",")]])

(def commands
  {"check-auth" check-auth
   "business-statements" business-statements
   "get-token" get-token
   "tax-report" tax-report})

(def help-msg
  [(string/join " " (concat ["Available arguments:"]
                            (keys commands)))])

(defn show-help [errors opts-summary]
  (doseq [line (concat errors help-msg [opts-summary])]
    (println line))
  [])

(defn parse-opts [args]
  (let [parsed-args (opts/parse-opts args cli-options)
        {:keys [options arguments errors summary]} parsed-args
        [cmd & cmd-args] arguments]
    (cond
      (not-empty errors) (show-help errors summary)
      (empty? arguments) (show-help [] summary)
      (not (contains? commands (str cmd))) (show-help [(str "Unknown command: " cmd)] summary)
      :else [cmd cmd-args options])))

(defn process-opts [options]
  (timbre/set-level! (if (:verbose options) :debug :warn)))

(defn process-cmd [cmd opts cmd-args]
  (apply (get commands cmd) opts cmd-args))

(defn -main
  "CLI entry point"
  [& args]
  (let [[cmd cmd-args opts :as all] (parse-opts args)]
    (when (not-empty all)
      (process-opts opts)
      (process-cmd cmd opts cmd-args))))
