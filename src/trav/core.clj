(ns trav.core
  (:gen-class)
  (:require [trav.chars :refer [make-living-character
                                format-name-map
                                format-skills
                                format-swag]]
            [trav.worlds.core :refer [gen-system
                                      system-str]]))


(defn -main [& [nstr & _]]
  #_(let [n (if nstr (Integer/parseInt nstr) 50)]
      (doseq [char (repeatedly n make-living-character)]
        (->> [(format-name-map char)
              (format-skills char)
              (format-swag char)]
             (remove empty?)
             (interpose "\n")
             (concat ["\n"])
             (apply str)
             println)))

  (let [char (make-living-character)
        world (gen-system)]
    (println "Your character:")
    (println "\t" (format-name-map char))
    (println "\t" (format-skills char))
    (println "\t" (format-swag char))
    (println "Your system:")
    (println (system-str world))))
