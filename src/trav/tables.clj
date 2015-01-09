(ns trav.tables
  (:require [trav.util :refer [keywordize]]))


;; Macro definitions for tables, etc.:
(defmacro defcoll [name & syms]
  `(def ~name (map keywordize (quote ~syms))))


(defn- row-vec [[service base-roll dms]]
  [(keyword service) {:base-roll (if (= base-roll '-)
                                   Double/POSITIVE_INFINITY
                                   base-roll)
                      :dms (map (fn [[attr thresh _ dm]]
                                  {:attr (keywordize attr)
                                   :thresh thresh
                                   :dm dm})
                                (partition 4 dms))}])


(defmacro def-service-table [tname & service-rows]
  `(do (def ~tname
         (->> (quote ~(partition 3 service-rows))
              (mapcat row-vec)
              (apply hash-map)))
       ~tname))


(defn selection-row-vec [[svc & elts]]
  [(keyword svc) (map #(if (= % '-) nil %) elts)])


(defmacro def-selection-table [tname & rows]
  `(do (def ~tname
         (->> '~rows
              (partition 7)
              (mapcat selection-row-vec)
              (apply hash-map)))
       ~tname))


(defmacro def-aging-table [tname & rows]
  `(do (def ~tname
         (->> '~rows
              (partition-by symbol?)
              (apply concat)
              (map (fn [s#] (if (symbol? s#) (keywordize s#) s#)))
              (apply hash-map)))
       ~tname))


(defmacro def-skill-table [tname & data]
  `(do
     (def ~tname
       (let [svcs# (map keywordize (take 6 '~data))
             rows# (->> '~data
                        (drop 6)
                        (partition 7)
                        (map (comp (partial map vector) rest)))]
         (->> rows#
              (map (partial interleave svcs#))
              (map (partial apply hash-map))
              (apply (partial merge-with (comp vec concat))))))
     ~tname))


(defcoll attributes ST DX EN IN ED SS)
(defcoll services navy marines army scouts merchant other)


(def-service-table enlistment
  navy     8 [IN 8 -> +1, ED 9 -> +2]
  marines  9 [IN 8 -> +1, ST 8 -> +2]
  army     5 [DX 6 -> +1, EN 5 -> +2]
  scouts   7 [IN 6 -> +1, ST 8 -> +2]
  merchant 7 [ST 7 -> +1, IN 6 -> +2]
  other    3 [])


(def-service-table survival
  navy     5 [IN 7 -> +2]
  marines  6 [EN 8 -> +2]
  army     5 [ED 6 -> +2]
  scouts   7 [EN 9 -> +2]
  merchant 5 [IN 7 -> +2]
  other    5 [IN 9 -> +2])


(def-service-table commission
  navy     10 [SS 9 -> +1]
  marines  9  [ED 7 -> +1]
  army     5  [EN 7 -> +1]
  scouts   -  []
  merchant 4  [IN 6 -> +1]
  other    -  [])


(def-service-table promotion
  navy     8  [ED 8 -> +1]
  marines  9  [SS 8 -> +1]
  army     6  [ED 7 -> +1]
  scouts   -  []
  merchant 10 [IN 9 -> +1]
  other    -  [])


(def-service-table reinlist
  navy     6 []
  marines  6 []
  army     7 []
  scouts   3 []
  merchant 4 []
  other    5 [])


(def-selection-table ranks
  navy     Ensign     Lieutenant LtCmdr    Commander Captain Admiral
  marines  Lieutenant Captain    ForceCmdr LtColonel Colonel Brigadier
  army     Lieutenant Captain    Major     LtColonel Colonel General
  scouts   -          -          -         -         -       -
  merchant FourthOffc ThirdOffc  SecndOffc FirstOffc Captain -
  other    -          -          -         -         -       -)


(def-aging-table aging
  ST {34 [-1 8]
      50 [-1 9]
      66 [-2 9]}
  DX {34 [-1 7]
      50 [-1 8]
      66 [-2 9]}
  EN {34 [-1 8]
      50 [-1 9]
      66 [-2 9]}
  IN {66 [-1 9]})


(def-skill-table personal-development-table
          navy   marines        army     scouts   merchant      other
  1       ST+1       ST+1       ST+1       ST+1       ST+1       ST+1
  2       DX+1       DX+1       DX+1       DX+1       DX+1       DX+1
  3       EN+1       EN+1       EN+1       EN+1       EN+1       EN+1
  4       SS+1   Gambling   Gambling     GunCbt       ST+1   BladeCbt
  5       IN+1   Brawling   Brawling       IN+1   BladeCbt   Brawling
  6       ED+1   BladeCbt       ED+1       ED+1   Brawling       SS-1)


(def-skill-table service-skills-table
          navy   marines        army     scouts   merchant      other
  1  ShipsBoat       ATV         ATV    AirRaft    Steward    Forgery
  2   VaccSuit  VaccSuit     AirRaft   VaccSuit   VaccSuit   Gambling
  3    FwdObsv  BladeCbt     FwdObsv Navigation       ST+1   Brawling
  4   BladeCbt  BladeCbt    BladeCbt Mechanical     GunCbt   BladeCbt
  5     GunCbt    GunCbt      GunCbt Electronic Electronic     GunCbt
  6    Gunnery    GunCbt      GunCbt   Jack-o-T   Jack-o-T    Bribery)


(def-skill-table advanced-education-table
          navy    marines       army     scouts   merchant      other
  1   VaccSuit        ATV        ATV    AirRaft Streetwise Streetwise
  2 Mechanical Mechanical Mechanical Mechanical Mechanical Mechanical
  3 Electronic Electronic Electronic Electronic Electronic Electronic
  4    Engnrng    Tactics    Tactics   Jack-o-T Navigation   Gambling
  5    Gunnery   BladeCbt   BladeCbt    Gunnery    Gunnery   Brawling
  6   Jack-o-T     GunCbt     GunCbt    Medical    Medical    Forgery)


(def-skill-table advanced-education-table-2
          navy    marines       army     scouts   merchant      other
  1    Medical    Medical    Medical    Medical    Medical    Medical
  2 Navigation    Tactics    Tactics Navigation Navigation    Forgery
  3    Engnrng    Tactics    Tactics    Engnrng    Engnrng Electronic
  4   Computer   Computer   Computer   Computer   Computer   Computer
  5      Pilot     Leader     Leader      Pilot      Pilot Streetwise
  6      Admin      Admin      Admin   Jack-o-T      Admin   Jack-o-T)
