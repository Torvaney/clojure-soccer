(defproject soccer-models "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.csv "0.1.4"]
                 [incanter "1.5.5"]]
  :main soccer-models.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
