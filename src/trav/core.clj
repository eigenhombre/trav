(ns trav.core
  (:gen-class)
  (:require [trav.char :refer [make-character format-name-map skills-str]]))


(defn -main [& [nstr & _]]
  (let [n (if nstr (Integer/parseInt nstr) 50)]
    (doseq [char (repeatedly 10 make-character)]
      (println (format-name-map char))
      (println (skills-str char))
      (println))))
