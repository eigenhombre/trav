(defproject trav/trav "0.0.1-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [eigenhombre/namejen "0.1.8"]
                 [uswitch/lambada "0.1.2"]
                 [cheshire "5.7.1"]]
  :aliases {"autotest" ["midje" ":autotest"]}
  :main trav.core
  :description "Riffing off Classic Traveller, a popular 80s-era SF RPG"
  :target-path "target/"
  :uberjar-name "trav.jar"
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[midje "1.6.3"]]
                   :plugins [[lein-midje "3.1.3"]
                             [com.jakemccrary/lein-test-refresh "0.20.0"]]}})
