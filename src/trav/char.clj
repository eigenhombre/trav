(ns trav.char
  (:require [trav.dice :refer [d]]
            [trav.util :refer [hexcode]]
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
(defn make-character []
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
     :rank 0
     :rank-name nil
     :name nom}))


(->> make-character
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
(->> make-character
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
          (assoc :rank-name rank-name))
      char)))


(defn maybe-promote [char]
  (cond
   (not (:living? char)) char
   (:commissioned? char) (if (roll-for-service-table-succeeds? promotion char)
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


(defn apply-term-of-service [char]
  (-> char
      maybe-kill
      maybe-promote
      age))


(->> (make-character)
     (iterate apply-term-of-service)
     (take 10)
     vec)

;;=>
[{:actual-service :army,
  :age 18,
  :name "Alan Annon",
  :commissioned? false,
  :living? true,
  :rank 0,
  :drafted? false,
  :rank-name nil,
  :desired-service :army,
  :gender :male,
  :attributes {:ss 5, :ed 5, :in 11, :en 7, :dx 5, :st 4}}
 {:actual-service :army,
  :age 22,
  :name "Alan Annon",
  :commissioned? true,
  :living? true,
  :rank 1,
  :drafted? false,
  :rank-name Lieutenant,
  :desired-service :army,
  :gender :male,
  :attributes {:ss 5, :ed 5, :in 11, :en 7, :dx 5, :st 4}}
 {:actual-service :army,
  :age 26,
  :name "Alan Annon",
  :commissioned? true,
  :living? true,
  :rank 1,
  :drafted? false,
  :rank-name Lieutenant,
  :desired-service :army,
  :gender :male,
  :attributes {:ss 5, :ed 5, :in 11, :en 7, :dx 5, :st 4}}
 {:actual-service :army,
  :age 30,
  :name "Alan Annon",
  :commissioned? true,
  :living? true,
  :rank 1,
  :drafted? false,
  :rank-name Lieutenant,
  :desired-service :army,
  :gender :male,
  :attributes {:ss 5, :ed 5, :in 11, :en 7, :dx 5, :st 4}}
 {:actual-service :army,
  :age 34,
  :name "Alan Annon",
  :commissioned? true,
  :living? true,
  :rank 2,
  :drafted? false,
  :rank-name Captain,
  :desired-service :army,
  :gender :male,
  :attributes {:ss 5, :ed 5, :in 11, :en 7, :dx 5, :st 4}}
 {:actual-service :army,
  :age 38,
  :name "Alan Annon",
  :commissioned? true,
  :living? true,
  :rank 3,
  :drafted? false,
  :rank-name Major,
  :desired-service :army,
  :gender :male,
  :attributes {:ss 5, :ed 5, :in 11, :en 7, :dx 5, :st 4}}
 {:actual-service :army,
  :age 42,
  :name "Alan Annon",
  :commissioned? true,
  :living? true,
  :rank 4,
  :drafted? false,
  :rank-name LtColonel,
  :desired-service :army,
  :gender :male,
  :attributes {:ss 5, :ed 5, :in 11, :en 7, :dx 5, :st 4}}
 {:actual-service :army,
  :age 46,
  :name "Alan Annon",
  :commissioned? true,
  :living? true,
  :rank 4,
  :drafted? false,
  :rank-name LtColonel,
  :desired-service :army,
  :gender :male,
  :attributes {:ss 5, :ed 5, :in 11, :en 7, :dx 5, :st 4}}
 {:actual-service :army,
  :age 50,
  :name "Alan Annon",
  :commissioned? true,
  :living? true,
  :rank 4,
  :drafted? false,
  :rank-name LtColonel,
  :desired-service :army,
  :gender :male,
  :attributes {:ss 5, :ed 5, :in 11, :en 7, :dx 5, :st 4}}
 {:actual-service :army,
  :age 54,
  :name "Alan Annon",
  :commissioned? true,
  :living? true,
  :rank 4,
  :drafted? false,
  :rank-name LtColonel,
  :desired-service :army,
  :gender :male,
  :attributes {:ss 5, :ed 5, :in 11, :en 7, :dx 5, :st 4}}]
