(ns trav.lambda
  (:gen-class)
  (:require [uswitch.lambada.core :refer [deflambdafn]]
            [cheshire.core :as json]
            [trav.chars :refer [make-living-character]]))


(deflambdafn trav.lambda.Hello [in out ctx]
  (let [ret-json (json/generate-string
                  (repeatedly 5 make-living-character))]
    (json/generate-stream
     {:isBase64Encoded false
      :statusCode 200
      :headers {:Content-Type "application/json"}
      :body ret-json}
     (clojure.java.io/writer out))))
