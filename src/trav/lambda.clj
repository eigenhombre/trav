(ns trav.lambda
  (:gen-class)
  (:require [cheshire.core :as json]
            [clojure.pprint]
            [clojure.walk]
            [trav.chars :as chr]
            [uswitch.lambada.core :refer [deflambdafn]]))


(defn lambda-output [n lang]
  (condp = lang
    "english" (->> (for [char (repeatedly n chr/make-living-character)]
                     (->> [(chr/format-name-map char)
                           (chr/format-skills char)
                           (chr/format-swag char)]
                          (remove empty?)
                          (interpose "\n")
                          (concat ["\n\n"])))
                   concat
                   (apply concat)
                   (apply str))
    "edn" (with-out-str
            (clojure.pprint/pprint
             {:characters (repeatedly n chr/make-living-character)}))
    (json/generate-string
     {:characters (repeatedly n chr/make-living-character)})))


(deflambdafn trav.lambda.Characters [in out ctx]
  (let [params (->> in
                    slurp
                    json/parse-string
                    clojure.walk/keywordize-keys
                    :queryStringParameters)
        language (:lang params)
        n (or (some-> params :n Integer.) 10)]
      (json/generate-stream
     {:statusCode 200
      :headers {"Content-Type"
                (condp = language
                    "english" "text/plain"
                    "edn" "text/plain"
                    "application/json")}
      :body (lambda-output n language)}
     (clojure.java.io/writer out)
     {:pretty true})))
