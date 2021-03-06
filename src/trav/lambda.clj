(ns trav.lambda
  (:gen-class)
  (:require [cheshire.core :as json]
            [clojure.pprint]
            [clojure.walk]
            [trav.chars :as chr]
            [trav.worlds.core :as w]
            [uswitch.lambada.core :refer [deflambdafn]]))


(defn lambda-output [type_ n lang]
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
     {:characters (repeatedly n chr/make-living-character)}
     {:pretty true})))


(deflambdafn trav.lambda.Characters [in out ctx]
  (let [params (some->> in
                        slurp
                        json/parse-string
                        clojure.walk/keywordize-keys
                        :queryStringParameters)
        language (:lang params)
        n (or (some-> params :n Integer.) 10)
        type_ (or (get params :type) "chars")]
    (json/generate-stream
     {:statusCode 200
      :headers {"Content-Type"
                (condp = language
                  "english" "text/plain"
                  "edn" "text/plain"
                  "application/json")}
      :body (lambda-output type_ n language)}
     (clojure.java.io/writer out)
     {:pretty true})))
