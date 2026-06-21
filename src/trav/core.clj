(ns trav.core
  (:gen-class)
  (:require [clojure.string :as str]
            [trav.chars :refer [make-living-character
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

(defn char-str [character]
  (str/join "\n\t" [(format-name-map character)
                    (format-skills character)
                    (format-swag character)]))

(defn -main [& [arg]]
  (let [char (make-living-character)
        system (gen-system)]
    (cond
      (#{"char" "chr" "character"} arg)
      (println (char-str char))

      (#{"sys" "system"} arg)
      (println (system-str system))

      :else
      (do
        (println "Your character:")
        (println (char-str char))
        (println "\n\n\n")
        (println "Your system:")
        (println (system-str system))
        (println "\nYour current location:" (find-character-location system))))))

(comment
  (-main)

  (-main "char")
  (-main "sys"))
