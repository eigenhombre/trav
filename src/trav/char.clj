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


(defcoll attributes ST DX EN IN ED SS)
(defcoll services navy marines army scouts merchant other)


(defn char-attr-map []
  (zipmap attributes (take (count attributes)
                           (repeatedly d))))


(defn name-maker [] (first (funny-name-maker)))



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
     :name nom}))


(->> make-character
     repeatedly
     (take 10)
     vec)

;;=>
[{:age 18,
  :gender :female,
  :attributes {:ss 12, :ed 6, :in 3, :en 7, :dx 7, :st 7},
  :desired-service :scouts,
  :actual-service :scouts,
  :drafted? false,
  :living? true,
  :name "Von Miss Yllos Ltos, LCPT"}
 {:age 18,
  :gender :female,
  :attributes {:ss 9, :ed 8, :in 7, :en 7, :dx 4, :st 7},
  :desired-service :navy,
  :actual-service :navy,
  :drafted? false,
  :living? true,
  :name "Jeri Oore"}
 {:age 18,
  :gender :male,
  :attributes {:ss 11, :ed 10, :in 4, :en 5, :dx 7, :st 6},
  :desired-service :marines,
  :actual-service :navy,
  :drafted? true,
  :living? true,
  :name "Sir Inos Roze"}
 {:age 18,
  :gender :female,
  :attributes {:ss 8, :ed 7, :in 10, :en 6, :dx 8, :st 11},
  :desired-service :army,
  :actual-service :army,
  :drafted? false,
  :living? true,
  :name "Aurie Meehan V"}
 {:age 18,
  :gender :female,
  :attributes {:ss 7, :ed 8, :in 3, :en 6, :dx 4, :st 8},
  :desired-service :other,
  :actual-service :other,
  :drafted? false,
  :living? true,
  :name "Effrey Kevin Urtis Lius Hsuan"}
 {:age 18,
  :gender :male,
  :attributes {:ss 3, :ed 8, :in 4, :en 7, :dx 8, :st 10},
  :desired-service :navy,
  :actual-service :navy,
  :drafted? false,
  :living? true,
  :name "Allan Athnakumar, LMA"}
 {:age 18,
  :gender :male,
  :attributes {:ss 7, :ed 11, :in 10, :en 6, :dx 8, :st 4},
  :desired-service :marines,
  :actual-service :scouts,
  :drafted? true,
  :living? true,
  :name "Gail Stlik Daresh, Ph.D."}
 {:age 18,
  :gender :other,
  :attributes {:ss 6, :ed 9, :in 8, :en 4, :dx 8, :st 3},
  :desired-service :other,
  :actual-service :other,
  :drafted? false,
  :living? true,
  :name "Gger Alus Enkata Evilles Eruyuki"}
 {:age 18,
  :gender :male,
  :attributes {:ss 7, :ed 5, :in 12, :en 8, :dx 10, :st 7},
  :desired-service :merchant,
  :actual-service :marines,
  :drafted? true,
  :living? true,
  :name "Helen Ckey"}
 {:age 18,
  :gender :male,
  :attributes {:ss 6, :ed 2, :in 5, :en 4, :dx 9, :st 4},
  :desired-service :navy,
  :actual-service :other,
  :drafted? true,
  :living? true,
  :name "Lyde Edward"}]


(defprotocol UPP
  (upp [this]))


(extend-protocol UPP
  clojure.lang.PersistentArrayMap
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


(defn determine-commission [char]
  (if (:commissioned? char)
    true
    (let [stats (:attributes char)
          {:keys [base-roll dms]} (->> char
                                       :actual-service
                                       (#(commission %)))]
      (roll-with-dms-succeeds? base-roll dms stats))))


(defn apply-term-of-service [char]
  (if-not (:living? char)
    char
    (if-not (survived-term? char)
      (-> char
          (assoc :living? false)
          (update-in [:age] + (rand-int 5)))
      (-> char
          (update-in [:age] + 4)
          (assoc :commissioned? (determine-commission char))))))
