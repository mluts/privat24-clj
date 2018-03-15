(defproject privat24-clj "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/tools.cli "0.3.5"]
                 [clj-http "3.7.0"]
                 [clj-http-fake "1.0.3"]
                 [clj-time "0.14.2"]
                 [cheshire "5.8.0"]
                 [slingshot "0.12.2"]
                 [medley "1.0.0"]
                 [com.taoensso/timbre "4.10.0"]]
  :main ^:skip-aot privat24-clj.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
