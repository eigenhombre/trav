(ns build
  (:require [clojure.tools.build.api :as b]))

(def lib 'trav/trav)
(def version "0.1.0")
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def uber-file "trav.jar")

(defn clean [_]
  (b/delete {:path "target"})
  (b/delete {:path "trav.jar"}))

(defn uber [_]
  (clean nil)
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (b/compile-clj {:basis basis
                  :ns-compile '[trav.core]
                  :class-dir class-dir})
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis basis
           :main 'trav.core}))
