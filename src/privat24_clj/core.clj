(ns privat24-clj.core
  (:require [privat24-clj.cli :as cli]
            [privat24-clj.util :refer [->json]]
            [clojure.tools.cli :as opts]
            [clojure.string :as string]
            [taoensso.timbre :as timbre])
  (:gen-class))

(defn check-auth []
  (let [[res err] (cli/check-auth)]
    (if res
      (println "OK")
      (println "ERR: " err))))

(defn business-statements [date-start date-end]
  (let [[res err] (cli/business-statements date-start date-end)]
    (if err
      (println "ERR: " err)
      (println (->json res)))))

(def cli-options
  [["-v" "--verbose"]])

(def commands
  {"check-auth" check-auth
   "business-statements" business-statements})

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

(defn process-cmd [cmd cmd-args]
  (apply (get commands cmd) cmd-args))

(defn -main
  "CLI entry point"
  [& args]
  (let [[cmd cmd-args opts :as all] (parse-opts args)]
    (when (not-empty all)
      (process-opts opts)
      (process-cmd cmd cmd-args))))
