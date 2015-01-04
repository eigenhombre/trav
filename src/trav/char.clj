(ns trav.char
  (:require [trav.dice :refer [d]]
            [trav.util :refer [hexcode take-until]]
            [namejen.names :refer [gen-name-data-as-map]]))


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


(defn starting-character []
  (let [stats (char-attr-map)
        soc (get stats :ss)
        knighted? (= soc 11)
        baron? (= soc 12)
        royal-form (cond knighted? "Sir"
                         baron? "Von")
        [desired-service drafted? actual-service] (determine-service stats)]
    (merge (gen-name-data-as-map)
           {:age 18
            :royal-form royal-form
            :attributes stats
            :desired-service desired-service
            :actual-service actual-service
            :drafted? drafted?
            :living? true
            :commissioned? false
            :reinlisting? true
            :rank 0
            :rank-name nil})))


(->> starting-character
     repeatedly
     (take 4)
     vec)

;;=>
'[{:royal-form nil,
   :reinlisting? true,
   :actual-service :scouts,
   :generation nil,
   :age 18,
   :commissioned? false,
   :living? true,
   :rank 0,
   :first-name "Nthony",
   :surnames ("Rnest"),
   :prefix nil,
   :drafted? false,
   :rank-name nil,
   :desired-service :scouts,
   :gender :male,
   :attributes {:ss 6, :ed 8, :in 8, :en 10, :dx 5, :st 3}}
  {:royal-form nil,
   :reinlisting? true,
   :actual-service :other,
   :generation nil,
   :age 18,
   :commissioned? false,
   :living? true,
   :rank 0,
   :first-name "Atoshiko",
   :surnames ("Anislaw" "Ndro"),
   :prefix nil,
   :drafted? false,
   :rank-name nil,
   :desired-service :other,
   :gender :female,
   :attributes {:ss 4, :ed 11, :in 8, :en 9, :dx 11, :st 4}}
  {:royal-form nil,
   :reinlisting? true,
   :actual-service :other,
   :generation nil,
   :age 18,
   :commissioned? false,
   :living? true,
   :rank 0,
   :first-name "Lendolynn",
   :surnames (),
   :prefix nil,
   :drafted? false,
   :rank-name nil,
   :desired-service :other,
   :gender :female,
   :attributes {:ss 8, :ed 6, :in 7, :en 7, :dx 4, :st 12}}
  {:royal-form "Von",
   :reinlisting? true,
   :actual-service :other,
   :generation nil,
   :age 18,
   :commissioned? false,
   :living? true,
   :rank 0,
   :first-name "Tancenza",
   :surnames ("Anavendran" "Nadeem"),
   :prefix nil,
   :drafted? true,
   :rank-name nil,
   :desired-service :merchant,
   :gender :female,
   :attributes {:ss 12, :ed 8, :in 5, :en 2, :dx 6, :st 7}}]


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


;; Adapted from eigenhombre/namejen:
(defn format-name-map [{:keys [gender
                               prefix
                               first-name
                               surnames
                               generation
                               actual-service
                               rank-name
                               royal-form
                               attributes] :as char}]
  (let [prefix (if rank-name rank-name prefix)]
    (apply str `(~@(if (and prefix
                            (seq surnames)) [prefix " "])
                 ~@ (if (and royal-form
                             (seq surnames)) [royal-form " "])
                    ~first-name
                    ~(if (seq surnames) " " "")
                    ~@(interpose " " surnames)
                    ~@(if (and (seq surnames)
                               generation) [", " generation])
                    ~({:other ", " :male " (M), " :female " (F), "} gender)
                    ~(if (= actual-service :other)
                       ""
                       (str (name actual-service) ", "))
                    ~(upp char)))))


;; Example - character names + UPPs:
(->> starting-character
     repeatedly
     (take 10)
     (map format-name-map)
     vec)

;;=>
["Ms. Lady Debi (F), merchant, 895849"
 "Emona Anan (F), marines, 87CB8A"
 "Mr. Nicholas Erman, III (M), 3B8393"
 "Ayson Joon (M), 878436"
 "Ms. Sir Ieko Bson (F), marines, 727A6B"
 "Kasandelany Orraine Ised Orne Hakil (F), merchant, 6289A7"
 "Ms. Osphina Hatter Loukas (F), army, 788849"
 "Sir Librada Will Luke Presley (F), navy, 75746B"
 "Ms. Noma Lexis (F), scouts, 45BA7A"
 "Oward Murat (M), army, B86744"]


;; Example - full characters with name, rank, age and UPP:
(->> make-character
     (repeatedly 50)
     (remove (complement :living?))  ;; Bring out yer dead!!!
     (sort-by :name)
     (map format-name-map)
     vec)

;;=>
["Grette Aime (F), scouts, 64A785"
 "Orth Rman (M), 8887A5"
 "Srta. Eranza Dward (F), 4A7454"
 "FourthOffc Modessa Evilles Oshua Jayesh (F), merchant, 85A423"
 "Ms. Sir Jenelorelei Enis (F), scouts, 87A84B"
 "Jaquel Lance Yung Pontus Tommy (F), A68C88"
 "Sra. Valyn Vern (F), scouts, A79699"
 "Lord Ngus (M), scouts, 78A873"
 "FourthOffc Ugio Reid Olis Sell Kelvin (F), merchant, 7BCA77"
 "Sir Rashad Stewart (M), navy, 9B4955"
 "Elease (F), scouts, 6CB754"
 "Umbertram Acobson Vier Marek Anklin (M), 45A948"
 "Lieutenant Arrencio Undar (M), army, 478584"
 "FourthOffc Sir Chung Jimmy, I (M), merchant, 83A93B"
 "Ms. Sir Linnea Igel (F), 89C36B"
 "Eddy Ablo, IV (M), 5897C2"
 "Zalo Huvra (M), 888883"
 "LtCmdr Amon Mawan Lars, I (M), navy, 2A7BA7"
 "Ms. Undra Ared (F), 786597"
 "Lieutenant Dger Amsey (M), army, 54A679"
 "Sell Reiner (M), 577963"
 "FourthOffc Handrina Pyros (F), merchant, 58AB93"
 "Udio (M), army, 879878"
 "Captain Utumn Anacea (F), army, A39996"
 "ThirdOffc Bret Mesh, Jr. (M), merchant, 665477"
 "Fr. Tonet Eckie Vijay (F), 797947"
 "Captain Articia Ajesh (F), marines, C76839"
 "Sica (F), 853B79"
 "Hestefana Ofer (F), army, 5463A5"
 "FirstOffc Icaela Oleen (F), merchant, 382984"
 "Captain Melonia Kayuki Elberto (F), army, C6A585"
 "Maire Jinch Hael (F), scouts, 438968"
 "Sir Hery Ughn Iber Ederic Indy (F), marines, 8C683B"
 "Lieutenant Aryrosenaida Agnus (F), army, 747985"
 "Lieutenant Ight Galentinos June (M), army, 98B872"]
