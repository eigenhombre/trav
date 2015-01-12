(ns trav.core
  (:gen-class)
  (:require [trav.chars :refer [make-living-character
                                format-name-map
                                format-skills
                                format-swag]]))


(defn -main [& [nstr & _]]
  (let [n (if nstr (Integer/parseInt nstr) 50)]
    (doseq [char (repeatedly n make-living-character)]
      (->> [(format-name-map char)
            (format-skills char)
            (format-swag char)]
       (remove empty?)
       (interpose "\n")
       (concat ["\n"])
       (apply str)
       println))))

