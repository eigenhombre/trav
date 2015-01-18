(defproject trav/trav "0.0.1-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [eigenhombre/namejen "0.1.8"]]
  :aliases {"autotest" ["midje" ":autotest"]}
  :main trav.core
  :description "Riffing off Classic Traveller, a popular 80s-era SF RPG"
  :profiles {:dev {:dependencies [[midje "1.6.3"]]
                   :plugins [[lein-midje "3.1.3"]]}
             :uberjar {:aot :all}})
