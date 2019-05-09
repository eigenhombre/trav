(ns trav.core
  (:gen-class)
  (:require [trav.chars :refer [make-living-character
                                format-name-map
                                format-skills
                                format-swag]]
            [trav.worlds.core :refer [gen-system
                                      system-str]]))


(defn find-character-location [system]
  (let [satellite-fields (comp (partial mapcat
                                        (juxt :population :name_))
                               :satellites)
        planet-or-belt-fields (comp (partial mapcat
                                             (juxt :population :name_))
                                    :orbits)
        satellite-pops (mapcat (comp (partial mapcat satellite-fields)
                                     :orbits)
                               system)
        planet-or-belt-pops (mapcat planet-or-belt-fields system)]
    (rand-nth
     (apply concat (for [[n name_]
                         (->> planet-or-belt-pops
                              (concat satellite-pops)
                              (partition 2)
                              (remove (comp nil? first))
                              (remove (comp zero? first)))]
                     (repeat n name_))))))


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
        system (gen-system)]
    (println "Your character:")
    (println "\t" (format-name-map char))
    (println "\t" (format-skills char))
    (println "\t" (format-swag char))
    (println "Your system:")
    (println (system-str system))
    (println "\nYour current location:" (find-character-location system))))
