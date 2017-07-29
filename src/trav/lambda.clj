(ns trav.lambda
  (:gen-class)
  (:require [uswitch.lambada.core :refer [deflambdafn]]
            [cheshire.core :as json]
            [trav.chars :refer [make-living-character]]))


(deflambdafn trav.lambda.Characters [in out ctx]
  (json/generate-stream
   {:characters
    (repeatedly 5 make-living-character)
    :in (pr-str (slurp in))}
   (clojure.java.io/writer out)
   {:pretty true}))
