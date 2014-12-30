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


;; Example - character names + UPPs:
(->> starting-character
     repeatedly
     (take 10)
     (map (juxt :name upp))
     vec)

;;=>
[["Tmann Kyle" "56AB45"]
 ["Dori Kate" "A78957"]
 ["Eehan Wade V" "B59854"]
 ["Sper Erri" "CAB664"]
 ["Sir Ugih Page" "479AA6"]
 ["Amsey Ndreas, LMA" "4956B7"]
 ["Floyd Einer V" "239837"]
 ["Opher Nklin" "6C488A"]
 ["Avis Wood" "893945"]
 ["Orbertrandi Thur Jr." "474459"]]


;; Example - full characters with name, rank, age and UPP:

(defn char-str [char]
  (let [rn (:rank-name char)
        died (not (:living? char))
        svc (:actual-service char)]
    (format "%s%s, %sAge %d %s%s"
            (if rn (str rn " ") "")
            (:name char)
            (if (= svc :other) "" (str (name svc) ", "))
            (:age char)
            (upp char)
            (if died " DIED" ""))))


(->> make-character
     (repeatedly 50)
     (remove (complement :living?))  ;; Bring out yer dead!!!
     (sort-by :name)
     (map char-str))

;;=>
("Anath Rice, Age 22 8465A8"
 "Ensign Anos Hector, navy, Age 22 64CA68"
 "Artyn, Age 22 A26758"
 "Captain Awan Chao, army, Age 30 887874"
 "FourthOffc Chael Hammad III, merchant, Age 22 BB98BA"
 "Curt Huashi, MD, Age 38 BAAB77"
 "Captain Cynthias Denis, LMT, army, Age 26 632CA7"
 "Elsa Lorrainer, scouts, Age 22 8894B7"
 "Lieutenant Eora Oyle, army, Age 22 64A3B6"
 "Lieutenant Exandeep Serdar Uerite, army, Age 22 857A76"
 "Lieutenant Harenda Oachim, army, Age 22 B68745"
 "Homas Iana, navy, Age 34 836B75"
 "General Ichiel Amie, army, Age 50 287945"
 "FourthOffc Ingbai Ancy, merchant, Age 22 A78677"
 "Kinchao Cher, army, Age 22 783464"
 "Kurt Suresh III, Age 26 379B7A"
 "FourthOffc Lowell Miya, merchant, Age 22 C86A83"
 "M. Lynneth Hierry, Age 22 366625"
 "Lieutenant Mone Osur, army, Age 22 65AA73"
 "LtColonel Mrs. Stevan Nette, army, Age 34 4B8974"
 "Mrs. Thleen Rion, Age 22 A8A486"
 "Ms. Aanandal Ffrey Anley, Esq., marines, Age 22 B885A4"
 "FourthOffc Nhard Douglas, merchant, Age 22 7466B9"
 "FourthOffc Nrad Dhar, merchant, Age 26 7B87B4"
 "Ensign Obbin Udsen, navy, Age 42 797A89"
 "Onella Alain, Age 22 9739B9"
 "Pablo Effery, marines, Age 26 A875B9"
 "Rahul Icole Shyam, Ph.D., Age 22 694444"
 "Romain Urph, scouts, Age 26 969858"
 "Sir Dawn Tewart, Age 30 28328B"
 "Sir Rgiu Atap V, Age 26 96B557"
 "Lieutenant Skip Regg, army, Age 22 667B38"
 "Lieutenant Sr. Ward Ilner, LCPT, army, Age 22 895A68"
 "Tarmi Egory Ephen, navy, Age 26 636379"
 "Major Thias Jouke, army, Age 34 75B885"
 "Captain Vadim Aleb II, army, Age 26 B957A5"
 "Ylan Roland, navy, Age 22 935C67")

