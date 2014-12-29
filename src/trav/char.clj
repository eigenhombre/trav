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


(defmacro deftable [tname & service-rows]
  `(do (def ~tname
         (->> (quote ~(partition 3 service-rows))
              (mapcat row-vec)
              (apply hash-map)))
       ~tname))


(defcoll attributes ST DX EN IN ED SS)
(defcoll services navy marines army scouts merchant other)


(deftable enlistment
  navy     8 [IN 8 -> +1, ED 9 -> +2]
  marines  9 [IN 8 -> +1, ST 8 -> +2]
  army     5 [DX 6 -> +1, EN 5 -> +2]
  scouts   7 [IN 6 -> +1, ST 8 -> +2]
  merchant 7 [ST 7 -> +1, IN 6 -> +2]
  other    3 [])


(deftable survival
  navy     5 [IN 7 -> +2]
  marines  6 [EN 8 -> +2]
  army     5 [ED 6 -> +2]
  scouts   7 [EN 9 -> +2]
  merchant 5 [IN 7 -> +2]
  other    5 [IN 9 -> +2])


(deftable commission
  navy     10 [SS 9 -> +1]
  marines  9  [ED 7 -> +1]
  army     5  [EN 7 -> +1]
  scouts   -  []
  merchant 4  [IN 6 -> +1]
  other    -  [])


(deftable promotion
  navy     8  [ED 8 -> +1]
  marines  9  [SS 8 -> +1]
  army     6  [ED 7 -> +1]
  scouts   -  []
  merchant 10 [IN 9 -> +1]
  other    -  [])


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


(->> make-character
     repeatedly
     (take 10)
     (map (juxt :name upp))
     vec)

;;=>
[["Tibartfast Ised" "2A9844"]
 ["Mr. Jisheng Ffie" "862A4A"]
 ["Miss Izchak Stic" "66A97A"]
 ["Sir Miss Dall Spock Hryn" "8B9B6B"]
 ["Mme. Kate" "779A56"]
 ["Vonne" "877768"]
 ["Arter" "8776A5"]
 ["M. Amos Nguyen, Esq." "97A6BA"]
 ["Liyuan Sarah" "A47677"]
 ["Shahid Jones" "8552B6"]]


;; Terms of service
(defn survived-term? [char]
  (let [stats (:attributes char)
        {:keys [base-roll dms]} (->> char
                                     :actual-service
                                     (#(survival %)))]
    (roll-with-dms-succeeds? base-roll dms stats)))


(defn commissioned? [char]
  (if (:commissioned? char)
    true
    (let [stats (:attributes char)
          {:keys [base-roll dms]} (->> char
                                       :actual-service
                                       (#(commission %)))]
      (roll-with-dms-succeeds? base-roll dms stats))))


(defn maybe-promote [char]
  (if-not (:commissioned? char)
    char
    (let [stats (:attributes char)
          {:keys [base-roll dms]} (->> char
                                       :actual-service
                                       (#(promotion %)))]
      (if-not (roll-with-dms-succeeds? base-roll dms stats)
        char
        (update-in char [:rank] inc)))))


(defn apply-term-of-service [char]
  (if-not (:living? char)
    char
    (if-not (survived-term? char)
      (-> char
          (assoc :living? false)
          (update-in [:age] + (rand-int 5)))
      (if (commissioned? char)
        (-> char
            (update-in [:age] + 4)
            (assoc :commissioned? true)
            maybe-promote)
        (-> char ;; Tough luck, survived but no promotion.
            (update-in [:age] + 4))))))


(->> (make-character)
     (iterate apply-term-of-service)
     (take-while :living?)
     vec)
;;=>
[{:actual-service :merchant,
  :age 18,
  :name "Rdad Kimberly III",
  :commissioned? false,
  :living? true,
  :rank 0,
  :drafted? false,
  :desired-service :merchant,
  :gender :female,
  :attributes {:ss 7, :ed 5, :in 8, :en 6, :dx 8, :st 8}}
 {:actual-service :merchant,
  :age 22,
  :name "Rdad Kimberly III",
  :commissioned? true,
  :living? true,
  :rank 0,
  :drafted? false,
  :desired-service :merchant,
  :gender :female,
  :attributes {:ss 7, :ed 5, :in 8, :en 6, :dx 8, :st 8}}
 {:actual-service :merchant,
  :age 26,
  :name "Rdad Kimberly III",
  :commissioned? true,
  :living? true,
  :rank 0,
  :drafted? false,
  :desired-service :merchant,
  :gender :female,
  :attributes {:ss 7, :ed 5, :in 8, :en 6, :dx 8, :st 8}}
 {:actual-service :merchant,
  :age 30,
  :name "Rdad Kimberly III",
  :commissioned? true,
  :living? true,
  :rank 0,
  :drafted? false,
  :desired-service :merchant,
  :gender :female,
  :attributes {:ss 7, :ed 5, :in 8, :en 6, :dx 8, :st 8}}
 {:actual-service :merchant,
  :age 34,
  :name "Rdad Kimberly III",
  :commissioned? true,
  :living? true,
  :rank 0,
  :drafted? false,
  :desired-service :merchant,
  :gender :female,
  :attributes {:ss 7, :ed 5, :in 8, :en 6, :dx 8, :st 8}}
 {:actual-service :merchant,
  :age 38,
  :name "Rdad Kimberly III",
  :commissioned? true,
  :living? true,
  :rank 0,
  :drafted? false,
  :desired-service :merchant,
  :gender :female,
  :attributes {:ss 7, :ed 5, :in 8, :en 6, :dx 8, :st 8}}
 {:actual-service :merchant,
  :age 42,
  :name "Rdad Kimberly III",
  :commissioned? true,
  :living? true,
  :rank 0,
  :drafted? false,
  :desired-service :merchant,
  :gender :female,
  :attributes {:ss 7, :ed 5, :in 8, :en 6, :dx 8, :st 8}}
 {:actual-service :merchant,
  :age 46,
  :name "Rdad Kimberly III",
  :commissioned? true,
  :living? true,
  :rank 1,
  :drafted? false,
  :desired-service :merchant,
  :gender :female,
  :attributes {:ss 7, :ed 5, :in 8, :en 6, :dx 8, :st 8}}
 {:actual-service :merchant,
  :age 50,
  :name "Rdad Kimberly III",
  :commissioned? true,
  :living? true,
  :rank 2,
  :drafted? false,
  :desired-service :merchant,
  :gender :female,
  :attributes {:ss 7, :ed 5, :in 8, :en 6, :dx 8, :st 8}}
 {:actual-service :merchant,
  :age 54,
  :name "Rdad Kimberly III",
  :commissioned? true,
  :living? true,
  :rank 2,
  :drafted? false,
  :desired-service :merchant,
  :gender :female,
  :attributes {:ss 7, :ed 5, :in 8, :en 6, :dx 8, :st 8}}
 {:actual-service :merchant,
  :age 58,
  :name "Rdad Kimberly III",
  :commissioned? true,
  :living? true,
  :rank 2,
  :drafted? false,
  :desired-service :merchant,
  :gender :female,
  :attributes {:ss 7, :ed 5, :in 8, :en 6, :dx 8, :st 8}}
 {:actual-service :merchant,
  :age 62,
  :name "Rdad Kimberly III",
  :commissioned? true,
  :living? true,
  :rank 3,
  :drafted? false,
  :desired-service :merchant,
  :gender :female,
  :attributes {:ss 7, :ed 5, :in 8, :en 6, :dx 8, :st 8}}
 {:actual-service :merchant,
  :age 66,
  :name "Rdad Kimberly III",
  :commissioned? true,
  :living? true,
  :rank 3,
  :drafted? false,
  :desired-service :merchant,
  :gender :female,
  :attributes {:ss 7, :ed 5, :in 8, :en 6, :dx 8, :st 8}}
 {:actual-service :merchant,
  :age 70,
  :name "Rdad Kimberly III",
  :commissioned? true,
  :living? true,
  :rank 3,
  :drafted? false,
  :desired-service :merchant,
  :gender :female,
  :attributes {:ss 7, :ed 5, :in 8, :en 6, :dx 8, :st 8}}
 {:actual-service :merchant,
  :age 74,
  :name "Rdad Kimberly III",
  :commissioned? true,
  :living? true,
  :rank 3,
  :drafted? false,
  :desired-service :merchant,
  :gender :female,
  :attributes {:ss 7, :ed 5, :in 8, :en 6, :dx 8, :st 8}}
 {:actual-service :merchant,
  :age 78,
  :name "Rdad Kimberly III",
  :commissioned? true,
  :living? true,
  :rank 3,
  :drafted? false,
  :desired-service :merchant,
  :gender :female,
  :attributes {:ss 7, :ed 5, :in 8, :en 6, :dx 8, :st 8}}
 {:actual-service :merchant,
  :age 82,
  :name "Rdad Kimberly III",
  :commissioned? true,
  :living? true,
  :rank 3,
  :drafted? false,
  :desired-service :merchant,
  :gender :female,
  :attributes {:ss 7, :ed 5, :in 8, :en 6, :dx 8, :st 8}}]


