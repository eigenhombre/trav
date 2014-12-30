(ns trav.char
  (:require [trav.dice :refer [d]]
            [trav.util :refer [hexcode take-until]]
            [namejen.names :refer [funny-name-maker]]))


(defn keywordize [s]
  (-> s
      name
      (#(.toLowerCase %))
      keyword))


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


(defn selection-row-vec [[svc & elts]]
  [(keyword svc) (map #(if (= % '-) nil %) elts)])


(defmacro def-selection-table [tname & rows]
  `(do (def ~tname
         (->> (quote ~rows)
              (partition 7)
              (mapcat selection-row-vec)
              (apply hash-map)))
       ~tname))


(def-selection-table ranks
  navy     Ensign     Lieutenant LtCmdr    Commander Captain Admiral
  marines  Lieutenant Captain    ForceCmdr LtColonel Colonel Brigadier
  army     Lieutenant Captain    Major     LtColonel Colonel General
  scouts   -          -          -         -         -       -
  merchant FourthOffc ThirdOffc  SecndOffc FirstOffc Captain -
  other    -          -          -         -         -       -)


(defn char-attr-map []
  (zipmap attributes (take (count attributes)
                           (repeatedly d))))


(defn name-maker [] (first (funny-name-maker)))


(defn determine-gender []
  (rand-nth (concat (repeat 10 :male)
                    (repeat 10 :female)
                    [:other])))


(defn roll-with-dms-succeeds? [base-roll dms stats]
  (let [applicable-dms (apply +
                              (for [{:keys [attr thresh dm]} dms
                                      :when (>= (stats attr) thresh)]
                                dm))]
    (>= (+ applicable-dms (d 2)) base-roll)))


(defn determine-service [stats]
  (let [desired-service (rand-nth services)
        service-dms (-> enlistment
                        desired-service
                        :dms)
        success? (roll-with-dms-succeeds?
                  (-> enlistment desired-service :base-roll)
                  service-dms
                  stats)
        actual-service (if success? desired-service (rand-nth services))]
    [desired-service (not success?) actual-service]))


;; FIXME: Add Ranks to names
(defn starting-character []
  (let [stats (char-attr-map)
        nom (name-maker)
        soc (get stats :ss)
        knighted? (= soc 11)
        baron? (= soc 12)
        nom (cond knighted? (str "Sir " nom)
                  baron? (str "Von " nom)
                  :else nom)
        [desired-service drafted? actual-service] (determine-service stats)]
    {:age 18
     :gender (determine-gender)
     :attributes stats
     :desired-service desired-service
     :actual-service actual-service
     :drafted? drafted?
     :living? true
     :commissioned? false
     :reinlisting? true
     :rank 0
     :rank-name nil
     :name nom}))


(->> starting-character
     repeatedly
     (take 4)
     vec)

;;=>
[{:actual-service :scouts,
  :age 18,
  :name "Suwandip Egory",
  :commissioned? false,
  :living? true,
  :rank 0,
  :drafted? false,
  :desired-service :scouts,
  :gender :female,
  :attributes {:ss 7, :ed 8, :in 7, :en 10, :dx 3, :st 5}}
 {:actual-service :marines,
  :age 18,
  :name "Lainer Think",
  :commissioned? false,
  :living? true,
  :rank 0,
  :drafted? false,
  :desired-service :marines,
  :gender :male,
  :attributes {:ss 7, :ed 6, :in 6, :en 5, :dx 4, :st 7}}
 {:actual-service :scouts,
  :age 18,
  :name "Dale Riam Erren",
  :commissioned? false,
  :living? true,
  :rank 0,
  :drafted? false,
  :desired-service :scouts,
  :gender :other,
  :attributes {:ss 6, :ed 11, :in 9, :en 5, :dx 4, :st 4}}
 {:actual-service :army,
  :age 18,
  :name "Atsan Itcher Danny Rcia Lenny",
  :commissioned? false,
  :living? true,
  :rank 0,
  :drafted? false,
  :desired-service :army,
  :gender :male,
  :attributes {:ss 5, :ed 12, :in 5, :en 4, :dx 4, :st 11}}]


(defprotocol UPP
  (upp [this]))


(extend-protocol UPP
  clojure.lang.PersistentHashMap
  (upp [this]
    (->> this
         :attributes
         (#(map % attributes))
         (map hexcode)
         (apply str))))


(defn as-syms [s] (vec (map symbol (clojure.string/split s #" "))))


;; Example - character names + UPPs:
(->> starting-character
     repeatedly
     (take 10)
     (map vec)
     vec)


(defn roll-for-service-table-succeeds? [table char]
  (let [stats (:attributes char)
        {:keys [base-roll dms]} (->> char
                                     :actual-service
                                     (#(table %)))]
    (roll-with-dms-succeeds? base-roll dms stats)))


;; Terms of service
(defn maybe-increase-rank [char]
  (let [rank-vals (vec (ranks (:actual-service char)))
        rank-name (get rank-vals (:rank char))]
    (if rank-name
      (-> char
          (update-in [:rank] inc)
          (assoc :rank-name (str rank-name)))
      char)))


(defn maybe-promote [char]
  (cond
   (not (:living? char)) char
   (:commissioned? char) (if (roll-for-service-table-succeeds?
                              promotion char)
                           (maybe-increase-rank char)
                           char)
   :else (if (roll-for-service-table-succeeds? commission char)
           (-> char
               (assoc :commissioned? true)
               maybe-increase-rank)
           char)))


(defn age [char]
  (if-not (:living? char)
    char
    (update-in char [:age] + 4)))


(defn maybe-kill [char]
  (if-not (:living? char)
    char
    (if (roll-for-service-table-succeeds? survival char)
      char
      (assoc char :living? false))))


(defn maybe-reinlist [char]
  (if-not (:living? char)
    char
    (let [wants-to-reinlist (rand-nth [true true true false])
          stats (:attributes char)
          {:keys [base-roll _]} (->> char
                                     :actual-service
                                     (#(reinlist %)))
          roll (d 2)
          reinlisting? (or (= roll 12)
                           (and wants-to-reinlist (>= roll base-roll)))]
      (assoc char :reinlisting? reinlisting?))))


(defn apply-term-of-service [char]
  (-> char
      maybe-kill
      maybe-promote
      maybe-reinlist
      ;; TODO: skills
      ;; TODO: posessions
      ;; TODO: aging
      age))


(defn make-character []
  (->> (starting-character)
       (iterate apply-term-of-service)
       (take-until (fn [m] (or (not (:reinlisting? m))
                               (not (:living? m)))))
       last))

(vec (repeatedly 20 make-character))

;;=>
[{:reinlisting? false,
  :actual-service :other,
  :age 22,
  :name "Dr. Ickey Erik Kinchao, LMA",
  :commissioned? false,
  :living? true,
  :rank 0,
  :drafted? false,
  :rank-name nil,
  :desired-service :other,
  :gender :female,
  :attributes {:ss 8, :ed 8, :in 6, :en 7, :dx 7, :st 8}}
 {:reinlisting? true,
  :actual-service :scouts,
  :age 18,
  :name "Kemal Ones N-pierette I",
  :commissioned? false,
  :living? false,
  :rank 0,
  :drafted? false,
  :rank-name nil,
  :desired-service :scouts,
  :gender :male,
  :attributes {:ss 7, :ed 5, :in 7, :en 12, :dx 10, :st 7}}
 {:reinlisting? true,
  :actual-service :army,
  :age 18,
  :name "Herr Lijah Ndries",
  :commissioned? false,
  :living? false,
  :rank 0,
  :drafted? false,
  :rank-name nil,
  :desired-service :army,
  :gender :male,
  :attributes {:ss 7, :ed 3, :in 11, :en 11, :dx 10, :st 12}}
 {:reinlisting? false,
  :actual-service :marines,
  :age 22,
  :name "Yros Thew",
  :commissioned? false,
  :living? true,
  :rank 0,
  :drafted? false,
  :rank-name nil,
  :desired-service :marines,
  :gender :female,
  :attributes {:ss 7, :ed 3, :in 7, :en 8, :dx 9, :st 9}}
 {:reinlisting? false,
  :actual-service :other,
  :age 30,
  :name "Laclypse",
  :commissioned? false,
  :living? true,
  :rank 0,
  :drafted? false,
  :rank-name nil,
  :desired-service :other,
  :gender :male,
  :attributes {:ss 7, :ed 2, :in 10, :en 6, :dx 4, :st 7}}
 {:reinlisting? false,
  :actual-service :scouts,
  :age 22,
  :name "Leria Enry, LMT",
  :commissioned? false,
  :living? true,
  :rank 0,
  :drafted? false,
  :rank-name nil,
  :desired-service :scouts,
  :gender :female,
  :attributes {:ss 10, :ed 6, :in 11, :en 7, :dx 11, :st 2}}
 {:reinlisting? false,
  :actual-service :merchant,
  :age 66,
  :name "Elberto Pedro",
  :commissioned? true,
  :living? true,
  :rank 2,
  :drafted? false,
  :rank-name "ThirdOffc",
  :desired-service :merchant,
  :gender :male,
  :attributes {:ss 4, :ed 9, :in 7, :en 8, :dx 4, :st 3}}
 {:reinlisting? false,
  :actual-service :other,
  :age 22,
  :name "Justina Adriantaphyllos",
  :commissioned? false,
  :living? true,
  :rank 0,
  :drafted? false,
  :rank-name nil,
  :desired-service :other,
  :gender :male,
  :attributes {:ss 7, :ed 9, :in 6, :en 7, :dx 8, :st 11}}
 {:reinlisting? false,
  :actual-service :army,
  :age 22,
  :name "Herri",
  :commissioned? true,
  :living? true,
  :rank 1,
  :drafted? false,
  :rank-name "Lieutenant",
  :desired-service :army,
  :gender :male,
  :attributes {:ss 4, :ed 7, :in 7, :en 5, :dx 9, :st 8}}
 {:reinlisting? false,
  :actual-service :army,
  :age 22,
  :name "Stuart Merto Raphael, MD",
  :commissioned? true,
  :living? true,
  :rank 1,
  :drafted? true,
  :rank-name "Lieutenant",
  :desired-service :marines,
  :gender :male,
  :attributes {:ss 7, :ed 7, :in 3, :en 9, :dx 5, :st 8}}
 {:reinlisting? false,
  :actual-service :navy,
  :age 22,
  :name "Sanche Udio II",
  :commissioned? true,
  :living? true,
  :rank 1,
  :drafted? false,
  :rank-name "Ensign",
  :desired-service :navy,
  :gender :female,
  :attributes {:ss 9, :ed 7, :in 5, :en 11, :dx 7, :st 7}}
 {:reinlisting? false,
  :actual-service :merchant,
  :age 30,
  :name "Jochen Spike III",
  :commissioned? true,
  :living? true,
  :rank 1,
  :drafted? false,
  :rank-name "FourthOffc",
  :desired-service :merchant,
  :gender :female,
  :attributes {:ss 7, :ed 5, :in 7, :en 4, :dx 10, :st 6}}
 {:reinlisting? false,
  :actual-service :marines,
  :age 26,
  :name "Telisa Erat",
  :commissioned? true,
  :living? true,
  :rank 1,
  :drafted? false,
  :rank-name "Lieutenant",
  :desired-service :marines,
  :gender :male,
  :attributes {:ss 9, :ed 7, :in 10, :en 11, :dx 10, :st 8}}
 {:reinlisting? true,
  :actual-service :merchant,
  :age 18,
  :name "Rainer Vern Kathy, LCPT",
  :commissioned? false,
  :living? false,
  :rank 0,
  :drafted? false,
  :rank-name nil,
  :desired-service :merchant,
  :gender :female,
  :attributes {:ss 6, :ed 6, :in 4, :en 7, :dx 8, :st 3}}
 {:reinlisting? true,
  :actual-service :other,
  :age 22,
  :name "M. Olfe Natraj Minic, LMT",
  :commissioned? false,
  :living? false,
  :rank 0,
  :drafted? false,
  :rank-name nil,
  :desired-service :other,
  :gender :female,
  :attributes {:ss 7, :ed 5, :in 4, :en 5, :dx 12, :st 6}}
 {:reinlisting? true,
  :actual-service :scouts,
  :age 22,
  :name "Nandall Ritz Jr.",
  :commissioned? false,
  :living? false,
  :rank 0,
  :drafted? false,
  :rank-name nil,
  :desired-service :scouts,
  :gender :male,
  :attributes {:ss 10, :ed 9, :in 3, :en 9, :dx 9, :st 11}}
 {:reinlisting? false,
  :actual-service :navy,
  :age 22,
  :name "Rdre Pria, LMA",
  :commissioned? false,
  :living? true,
  :rank 0,
  :drafted? false,
  :rank-name nil,
  :desired-service :navy,
  :gender :female,
  :attributes {:ss 3, :ed 7, :in 11, :en 7, :dx 7, :st 7}}
 {:reinlisting? false,
  :actual-service :scouts,
  :age 22,
  :name "Sally Rley Ason",
  :commissioned? false,
  :living? true,
  :rank 0,
  :drafted? false,
  :rank-name nil,
  :desired-service :scouts,
  :gender :male,
  :attributes {:ss 6, :ed 11, :in 8, :en 9, :dx 8, :st 7}}
 {:reinlisting? false,
  :actual-service :army,
  :age 26,
  :name "Hirotoshi Eborah",
  :commissioned? true,
  :living? true,
  :rank 2,
  :drafted? false,
  :rank-name "Captain",
  :desired-service :army,
  :gender :male,
  :attributes {:ss 8, :ed 4, :in 3, :en 7, :dx 8, :st 4}}
 {:reinlisting? true,
  :actual-service :merchant,
  :age 18,
  :name "Ratt Napper",
  :commissioned? false,
  :living? false,
  :rank 0,
  :drafted? false,
  :rank-name nil,
  :desired-service :merchant,
  :gender :other,
  :attributes {:ss 8, :ed 5, :in 6, :en 6, :dx 6, :st 2}}]
