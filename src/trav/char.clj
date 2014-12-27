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



(defn- row-vec [[service roll-to-enlist dms]]
  [(keyword service) {:roll-to-enlist roll-to-enlist
                      :dms (map (fn [[attr thresh _ dm]]
                                  {:attr (keywordize attr)
                                   :thresh thresh
                                   :dm dm})
                                (partition 4 dms))}])


(defmacro deftable [tname & service-rows]
  `(def ~tname
     (->> (quote ~(partition 3 service-rows))
          (mapcat row-vec)
          (apply hash-map))))


(deftable enlistment
  navy     8 [IN 8 -> +1, ED 9 -> +2]
  marines  9 [IN 8 -> +1, ST 8 -> +2]
  army     5 [DX 6 -> +1, EN 5 -> +2]
  scouts   7 [IN 6 -> +1, ST 8 -> +2]
  merchant 7 [ST 7 -> +1, IN 6 -> +2]
  other    3 [])


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
        desired-service (rand-nth services)
        service-dms (-> enlistment
                        desired-service
                        :dms)
        applicable-dms (apply + (for [{:keys [attr thresh dm]} service-dms
                                      :when (>= (stats attr) thresh)]
                                  dm))
        enlistment-roll (d 2)
        success? (>= (+ applicable-dms enlistment-roll)
                     (-> enlistment desired-service :roll-to-enlist))
        actual-service (if success? desired-service (rand-nth services))]
    ;(->Charactr stats 18 nom)
    {:attributes stats
     :desired-service desired-service
     :service-dms service-dms
     :applicable-dms applicable-dms
     :enlistment-roll enlistment-roll
     :successful-enlistment success?
     :actual-service actual-service
     :name nom}))


(take 10 (repeatedly make-character))

;;=>
[{:attributes {:ss 8, :ed 11, :in 4, :en 11, :dx 5, :st 10},
  :desired-service :navy,
  :service-dms
  ({:attr :in, :thresh 8, :dm 1} {:attr :ed, :thresh 9, :dm 2}),
  :applicable-dms 2,
  :enlistment-roll 7,
  :successful-enlistment true,
  :actual-service :navy,
  :name "Herr Nette Dorothy Nifer Kylo Ratap Jr."}
 {:attributes {:ss 6, :ed 7, :in 5, :en 6, :dx 7, :st 5},
  :desired-service :army,
  :service-dms
  ({:attr :dx, :thresh 6, :dm 1} {:attr :en, :thresh 5, :dm 2}),
  :applicable-dms 3,
  :enlistment-roll 7,
  :successful-enlistment true,
  :actual-service :army,
  :name "Tandy, LMA"}
 {:attributes {:ss 4, :ed 6, :in 7, :en 3, :dx 9, :st 10},
  :desired-service :navy,
  :service-dms
  ({:attr :in, :thresh 8, :dm 1} {:attr :ed, :thresh 9, :dm 2}),
  :applicable-dms 0,
  :enlistment-roll 5,
  :successful-enlistment false,
  :actual-service :marines,
  :name "Eonard III"}
 {:attributes {:ss 6, :ed 9, :in 8, :en 10, :dx 11, :st 3},
  :desired-service :navy,
  :service-dms
  ({:attr :in, :thresh 8, :dm 1} {:attr :ed, :thresh 9, :dm 2}),
  :applicable-dms 3,
  :enlistment-roll 10,
  :successful-enlistment true,
  :actual-service :navy,
  :name "Joni Trace"}
 {:attributes {:ss 7, :ed 7, :in 11, :en 6, :dx 7, :st 8},
  :desired-service :other,
  :service-dms (),
  :applicable-dms 0,
  :enlistment-roll 9,
  :successful-enlistment true,
  :actual-service :other,
  :name "Sir Julia Esper"}
 {:attributes {:ss 11, :ed 8, :in 5, :en 6, :dx 2, :st 8},
  :desired-service :other,
  :service-dms (),
  :applicable-dms 0,
  :enlistment-roll 9,
  :successful-enlistment true,
  :actual-service :other,
  :name "Sir N-christi Iete"}
 {:attributes {:ss 4, :ed 4, :in 8, :en 6, :dx 4, :st 11},
  :desired-service :army,
  :service-dms
  ({:attr :dx, :thresh 6, :dm 1} {:attr :en, :thresh 5, :dm 2}),
  :applicable-dms 2,
  :enlistment-roll 6,
  :successful-enlistment true,
  :actual-service :army,
  :name "Jingbai Louiqa, Esq."}
 {:attributes {:ss 4, :ed 7, :in 8, :en 4, :dx 6, :st 8},
  :desired-service :army,
  :service-dms
  ({:attr :dx, :thresh 6, :dm 1} {:attr :en, :thresh 5, :dm 2}),
  :applicable-dms 1,
  :enlistment-roll 8,
  :successful-enlistment true,
  :actual-service :army,
  :name "Sr. Nguyen Gideon Runo"}
 {:attributes {:ss 8, :ed 5, :in 7, :en 10, :dx 3, :st 7},
  :desired-service :merchant,
  :service-dms
  ({:attr :st, :thresh 7, :dm 1} {:attr :in, :thresh 6, :dm 2}),
  :applicable-dms 3,
  :enlistment-roll 10,
  :successful-enlistment true,
  :actual-service :merchant,
  :name "Miles Egge Avid"}
 {:attributes {:ss 5, :ed 10, :in 6, :en 7, :dx 7, :st 3},
  :desired-service :scouts,
  :service-dms
  ({:attr :in, :thresh 6, :dm 1} {:attr :st, :thresh 8, :dm 2}),
  :applicable-dms 1,
  :enlistment-roll 9,
  :successful-enlistment true,
  :actual-service :scouts,
  :name "Dhar Malcolm"}]


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

(map (juxt :name upp)
     (take 10 (repeatedly make-character)))

;;=>
(["Istin Ahmoud" "66987A"]
 ["Mayo Ordan" "485A6A"]
 ["Ndsey Sassan" "CB4895"]
 ["Mr. Wolf Eymour Lcolm" "A57768"]
 ["Iver Rice I" "575788"]
 ["Earnix Ogue II" "83954A"]
 ["Joyce Gela Jr." "8B7676"]
 ["Von Ocorrito Mmad Yanatoly, LCPT" "A5686C"]
 ["Dr. Nette Enora, LMA" "98BB96"]
 ["Sir Livier Rshi, LMA" "978638"])


