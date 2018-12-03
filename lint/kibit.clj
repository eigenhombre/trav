(ns kibit
  (:require [kibit.driver :refer [run]]
            [clojure.java.io :as io]))

(defn -main
  "
  ;; Adapted from https://github.com/jonase/kibit/issues/197...
  "
  []
  (run (for [folder ["src" "test"]
             :let [file-path (io/file folder)]
             :when (.exists file-path)]
         file-path)
    nil))
