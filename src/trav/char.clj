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


(defn as-syms [s] (vec (map symbol (clojure.string/split s #" "))))


;; Example - character names + UPPs:
(->> make-character
     repeatedly
     (take 10)
     (map (juxt (comp as-syms :name) (comp as-syms upp)))
     (map (partial apply concat))
     (map vec)
     vec)

;;=>
[[Unnard Ergio, MD 5374B4]
 [Llan Ivek C66C52]
 [Orton Thleen Judith 5755AA]
 [Hmet Mberly Dale A94696]
 [Ophe Jingbai Lentinos II 767967]
 [Sr. Nrichael Ustin 538489]
 [Sir Onal Eeks Udio 88999B]
 [Idney Rvillermo Assos Ierett Moore A84489]
 [Juri Vincenzo 983758]
 [Mme. Adley Nifer Sr. 849568]]



;; Terms of service
(defn maybe-commission [char]
  (cond
   (not (:living? char)) char
   (:commissioned? char) char
   :else (let [stats (:attributes char)
          {:keys [base-roll dms]} (->> char
                                       :actual-service
                                       (#(commission %)))]
           (if (roll-with-dms-succeeds? base-roll dms stats)
             (assoc char :commissioned? true)
             char))))


(defn maybe-promote [char]
  (cond
   (not (:living? char)) char
   (not (:commissioned? char)) char
   :else (let [stats (:attributes char)
          {:keys [base-roll dms]} (->> char
                                       :actual-service
                                       (#(promotion %)))]
           (if-not (roll-with-dms-succeeds? base-roll dms stats)
             char
             (update-in char [:rank] inc)))))


(defn age [char]
  (if-not (:living? char)
    char
    (update-in char [:age] + 4)))


(defn maybe-kill [char]
  (if-not (:living? char)
    char
    (let [stats (:attributes char)
          {:keys [base-roll dms]} (->> char
                                       :actual-service
                                       (#(survival %)))]
      (if (roll-with-dms-succeeds? base-roll dms stats)
        char
        (assoc char :living? false)))))


(defn apply-term-of-service [char]
  (-> char
      maybe-kill
      maybe-commission
      maybe-promote
      age))


(repeatedly 10 #(->> (make-character)
                     (iterate apply-term-of-service)
                     (take 10)
                     (map (juxt :name :living? :age :rank))
                     vec))

;;=>
([["Tigger, LCPT" true 18 0]
  ["Tigger, LCPT" true 22 0]
  ["Tigger, LCPT" true 26 0]
  ["Tigger, LCPT" true 30 0]
  ["Tigger, LCPT" true 34 0]
  ["Tigger, LCPT" true 38 0]
  ["Tigger, LCPT" false 38 0]
  ["Tigger, LCPT" false 38 0]
  ["Tigger, LCPT" false 38 0]
  ["Tigger, LCPT" false 38 0]]
 [["Azel Rnard Alan" true 18 0]
  ["Azel Rnard Alan" true 22 0]
  ["Azel Rnard Alan" true 26 0]
  ["Azel Rnard Alan" true 30 0]
  ["Azel Rnard Alan" true 34 0]
  ["Azel Rnard Alan" true 38 1]
  ["Azel Rnard Alan" true 42 1]
  ["Azel Rnard Alan" true 46 2]
  ["Azel Rnard Alan" true 50 2]
  ["Azel Rnard Alan" true 54 2]]
 [["Mrs. Ernie Wahar I" true 18 0]
  ["Mrs. Ernie Wahar I" true 22 0]
  ["Mrs. Ernie Wahar I" true 26 0]
  ["Mrs. Ernie Wahar I" false 26 0]
  ["Mrs. Ernie Wahar I" false 26 0]
  ["Mrs. Ernie Wahar I" false 26 0]
  ["Mrs. Ernie Wahar I" false 26 0]
  ["Mrs. Ernie Wahar I" false 26 0]
  ["Mrs. Ernie Wahar I" false 26 0]
  ["Mrs. Ernie Wahar I" false 26 0]]
 [["Dr. Hari Nolis, LCPT" true 18 0]
  ["Dr. Hari Nolis, LCPT" true 22 1]
  ["Dr. Hari Nolis, LCPT" true 26 1]
  ["Dr. Hari Nolis, LCPT" true 30 2]
  ["Dr. Hari Nolis, LCPT" true 34 2]
  ["Dr. Hari Nolis, LCPT" true 38 3]
  ["Dr. Hari Nolis, LCPT" true 42 4]
  ["Dr. Hari Nolis, LCPT" true 46 5]
  ["Dr. Hari Nolis, LCPT" true 50 6]
  ["Dr. Hari Nolis, LCPT" true 54 7]]
 [["Oshi Ques Fumi Olas Ussell" true 18 0]
  ["Oshi Ques Fumi Olas Ussell" true 22 1]
  ["Oshi Ques Fumi Olas Ussell" true 26 1]
  ["Oshi Ques Fumi Olas Ussell" true 30 1]
  ["Oshi Ques Fumi Olas Ussell" true 34 2]
  ["Oshi Ques Fumi Olas Ussell" true 38 3]
  ["Oshi Ques Fumi Olas Ussell" true 42 3]
  ["Oshi Ques Fumi Olas Ussell" true 46 4]
  ["Oshi Ques Fumi Olas Ussell" true 50 5]
  ["Oshi Ques Fumi Olas Ussell" true 54 6]]
 [["Ms. Rkeer, LMT" true 18 0]
  ["Ms. Rkeer, LMT" true 22 0]
  ["Ms. Rkeer, LMT" true 26 0]
  ["Ms. Rkeer, LMT" true 30 0]
  ["Ms. Rkeer, LMT" false 30 0]
  ["Ms. Rkeer, LMT" false 30 0]
  ["Ms. Rkeer, LMT" false 30 0]
  ["Ms. Rkeer, LMT" false 30 0]
  ["Ms. Rkeer, LMT" false 30 0]
  ["Ms. Rkeer, LMT" false 30 0]]
 [["Suwandip Avendra" true 18 0]
  ["Suwandip Avendra" false 18 0]
  ["Suwandip Avendra" false 18 0]
  ["Suwandip Avendra" false 18 0]
  ["Suwandip Avendra" false 18 0]
  ["Suwandip Avendra" false 18 0]
  ["Suwandip Avendra" false 18 0]
  ["Suwandip Avendra" false 18 0]
  ["Suwandip Avendra" false 18 0]
  ["Suwandip Avendra" false 18 0]]
 [["Ramsey IV" true 18 0]
  ["Ramsey IV" true 22 0]
  ["Ramsey IV" true 26 1]
  ["Ramsey IV" true 30 1]
  ["Ramsey IV" true 34 2]
  ["Ramsey IV" true 38 2]
  ["Ramsey IV" true 42 3]
  ["Ramsey IV" true 46 3]
  ["Ramsey IV" true 50 3]
  ["Ramsey IV" true 54 4]]
 [["Sir Mone Nest II" true 18 0]
  ["Sir Mone Nest II" true 22 0]
  ["Sir Mone Nest II" true 26 0]
  ["Sir Mone Nest II" true 30 1]
  ["Sir Mone Nest II" true 34 2]
  ["Sir Mone Nest II" true 38 3]
  ["Sir Mone Nest II" true 42 3]
  ["Sir Mone Nest II" false 42 3]
  ["Sir Mone Nest II" false 42 3]
  ["Sir Mone Nest II" false 42 3]]
 [["Eliott Danielle Hyllos, MD" true 18 0]
  ["Eliott Danielle Hyllos, MD" false 18 0]
  ["Eliott Danielle Hyllos, MD" false 18 0]
  ["Eliott Danielle Hyllos, MD" false 18 0]
  ["Eliott Danielle Hyllos, MD" false 18 0]
  ["Eliott Danielle Hyllos, MD" false 18 0]
  ["Eliott Danielle Hyllos, MD" false 18 0]
  ["Eliott Danielle Hyllos, MD" false 18 0]
  ["Eliott Danielle Hyllos, MD" false 18 0]
  ["Eliott Danielle Hyllos, MD" false 18 0]])






