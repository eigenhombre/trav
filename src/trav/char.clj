(ns trav.char
  (:require [trav.dice :refer [d]]
            [trav.util :refer [hexcode take-until]]
            [namejen.names :refer [gen-name-data-as-map]]))


;; Utilities:
(defn keywordize [s]
  (-> s
      name
      (#(.toLowerCase %))
      keyword))


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
                        (map (partial map str))
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


(def-skill-table advanced-education-table
          navy    marines       army     scouts   merchant      other
  1   VaccSuit        ATV        ATV    AirRaft Streetwise Streetwise
  2 Mechanical Mechanical Mechanical Mechanical Mechanical Mechanical
  3 Electronic Electronic Electronic Electronic Electronic Electronic
  4    Engnrng    Tactics    Tactics   Jack-o-T Navigation   Gambling
  5    Gunnery   BladeCbt   BladeCbt    Gunnery    Gunnery   Brawling
  6   Jack-o-T     GunCbt     GunCbt    Medical    Medical    Forgery)

;; ;;=>
;; {:other
;;  ["Streetwise" "Mechanical" "Electronic" "Gambling" "Brawling" "Forgery"],
;;  :navy
;;  ["VaccSuit" "Mechanical" "Electronic" "Engnrng" "Gunnery" "Jack-o-T"],
;;  :scouts
;;  ["AirRaft" "Mechanical" "Electronic" "Jack-o-T" "Gunnery" "Medical"],
;;  :army
;;  ["ATV" "Mechanical" "Electronic" "Tactics" "BladeCbt" "GunCbt"],
;;  :merchant
;;  ["Streetwise" "Mechanical" "Electronic" "Navigation" "Gunnery" "Medical"],
;;  :marines
;;  ["ATV" "Mechanical" "Electronic" "Tactics" "BladeCbt" "GunCbt"]}


;; Character determination:


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


(defn qualified-for-commission?
  "
  Draftees in their first term of service are not eligible for
  commissions.
  "
  [char]
  (or (not (:drafted? char))
      (> (:terms-reached char) 1)))


(defn maybe-promote [char]
  (cond
   (not (:living? char)) char
   (:commissioned? char) (if (roll-for-service-table-succeeds?
                              promotion char)
                           (maybe-increase-rank char)
                           char)
   :else (if (and (qualified-for-commission? char)
                  (roll-for-service-table-succeeds? commission char))
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


(defn handle-age-induced-illness
  "
  If attr drops below 1, see if death occurs (fail 8+ throw); if not,
  restore to 1.
  "
  [char attr]
  (let [a (-> char :attributes attr)]
    (cond
     (> a 0) char
     (>= (d) 8) (assoc-in char [:attributes attr] 1)
     :else (-> char
               (assoc :living? false)
               (assoc-in [:attributes attr] 0)))))


(defn apply-age-to-attribute [char attr]
  (let [age (:age char)
        {[delta throw-required] age} (attr aging)]
    (if (and delta (< (d) throw-required))
      (-> char
          (update-in [:attributes attr] + delta)
          (handle-age-induced-illness attr))
      char)))


(defn maybe-damage-for-age [char]
  (reduce apply-age-to-attribute char (keys aging)))


(defn age-one-year [char]
  (-> char
      (update-in [:age] inc)
      maybe-damage-for-age))


(defn age-one-term [char]
  (->> char
       (iterate age-one-year)
       (#(nth % 4))))


(defn increment-service-term [{:keys [terms-reached] :as char}]
  (if (nil? terms-reached)
    (assoc char :terms-reached 1)
    (update-in char [:terms-reached] inc)))


;; FIXME: add other tables:
(defn add-skill [{:keys [actual-service] :as char}]
  (let [skill (-> advanced-education-table actual-service rand-nth)]
    (update-in char [:skills] conj skill)))



(defn add-skills-for-service-term [{:keys [terms-reached] :as char}]
  (if (= terms-reached 1)
    (-> char add-skill add-skill)
    (-> char add-skill)))


(defn apply-term-of-service [char]
  (-> char
      increment-service-term
      add-skills-for-service-term
      maybe-kill
      maybe-promote
      maybe-reinlist
      age-one-term))
      ;; TODO: skills
      ;; TODO: posessions



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
                               age
                               actual-service
                               rank-name
                               royal-form
                               attributes] :as char}]
  (let [prefix (if rank-name rank-name prefix)]
    (apply str `(~@(when prefix [prefix " "])
                 ~@ (when (and royal-form
                               (seq surnames))
                      [royal-form " "])
                 ~first-name
                 ~(if (seq surnames) " " "")
                 ~@(interpose " " surnames)
                 ~@(when (and (seq surnames)
                              generation)
                     [", " generation])
                 ~({:other ", " :male " (M), " :female " (F), "} gender)
                 ~age " yrs. old, "
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
["Neida (F), 18 yrs. old, navy, 8837CA"
 "Pamala Lila Hadow, 18 yrs. old, scouts, 875667"
 "Miss Biola Dell Manjeev (F), 18 yrs. old, 6A6569"
 "Mael Ablo (M), 18 yrs. old, scouts, A44765"
 "Mr. Reyes Njay (M), 18 yrs. old, army, 685976"
 "Enee Iping Rkeer Dory Muel (F), 18 yrs. old, scouts, 927598"
 "Isha (M), 18 yrs. old, scouts, 7957A6"
 "Ms. Utta Miro Jussi Byron (F), 18 yrs. old, navy, 7768AA"
 "Nalistancey Trian Dwight Rtha (F), 18 yrs. old, 988789"
 "Lyde Indranath (M), 18 yrs. old, navy, 782665"]


;; Example - full characters with name, rank, age and UPP:
(->> make-character
     (repeatedly 50)
     (remove (complement :living?))  ;; Bring out yer dead!!!
     (sort-by :name)
     (map format-name-map)
     vec)

;;=>
["General Ster Urie, II (M), 50 yrs. old, army, 744688"
 "FourthOffc Sir Aydee Irving (F), 34 yrs. old, merchant, 555ABB"
 "Lieutenant Degarde Varda Arlie (F), 22 yrs. old, army, 74B627"
 "Rafina Imon (F), 22 yrs. old, scouts, 674877"
 "Lieutenant Parth Toku Irfan Ugih (F), 22 yrs. old, marines, B87A78"
 "Lieutenant Odette Wilmer Judith (F), 22 yrs. old, army, 898953"
 "ThirdOffc Rant Randi (M), 50 yrs. old, merchant, 822737"
 "Lieutenant Arby Ewart (F), 22 yrs. old, army, 466558"
 "Lieutenant Onse Mahesh (M), 22 yrs. old, army, 8888B6"
 "Mrs. Erdie Stacey (F), 58 yrs. old, scouts, 479797"
 "Mr. Kinley Alloy (M), 22 yrs. old, 395596"
 "Captain Iquel Robin (M), 30 yrs. old, marines, 774357"
 "Sebastian Jesper Dell (M), 22 yrs. old, navy, 7763A8"
 "SecndOffc Ambrose Ahid Ienz (M), 34 yrs. old, merchant, 687C48"
 "Mr. Hammed Aime Tagger (M), 26 yrs. old, 538C25"
 "FourthOffc Ises Rnard (M), 34 yrs. old, merchant, 5BA797"
 "Ms. Nelda Anaka Imawan (F), 22 yrs. old, scouts, A564A7"
 "Ensign Ulee (F), 22 yrs. old, navy, 75493A"
 "April Everly (F), 26 yrs. old, 577894"
 "Lieutenant Sir Field Raig Brooke, I (M), 22 yrs. old, army, 9887AB"
 "Major Dath Rgiu (F), 30 yrs. old, army, 686728"
 "Captain Trius Sorrell Fred Fletchel Dmond (M), 26 yrs. old, army, 887957"
 "Lieutenant Tangelique Ohong Ilot (F), 22 yrs. old, marines, 96B655"
 "Emarcus Lanny Uglas Orne (M), 22 yrs. old, marines, 4A6728"
 "SecndOffc Aurinda Atraj Rtis Tommy (F), 38 yrs. old, merchant, 877BAA"
 "Ensign Calvin Ronni Gnus, II (M), 26 yrs. old, navy, B97BB7"
 "Quinn Winston N-pierett Mothy, Sr. (M), 22 yrs. old, navy, 3A3B76"
 "Mr. Sreal Erre (M), 26 yrs. old, 864463"
 "Immie Llin Rich, Sr. (M), 26 yrs. old, 488235"
 "Captain Onio (F), 26 yrs. old, army, 8658A6"
 "FourthOffc Alizabethel Matt (F), 26 yrs. old, merchant, 47788A"
 "Lieutenant Thew Jared Lcolm (M), 22 yrs. old, army, 857779"
 "FourthOffc Ntonet Lliam Arty Jesper (F), 34 yrs. old, merchant, B74885"
 "Captain Etrius Chris Llan Eany Taurus (M), 26 yrs. old, army, 578669"
 "Captain Myong (F), 26 yrs. old, army, A87678"
 "Rrellena Anaka Rnard Alcolm Ahmet (F), 38 yrs. old, 62BB7A"
 "SecndOffc Sabet Daresan (F), 38 yrs. old, merchant, 47AAB5"]


;; Effects of aging... note UPP:
(->> (starting-character)
     (iterate age-one-term)
     (take 20)
     (map format-name-map)
     vec)


;;=>
["Kierstenesha Rolin (F), 18 yrs. old, army, 588747"
 "Kierstenesha Rolin (F), 22 yrs. old, army, 588747"
 "Kierstenesha Rolin (F), 26 yrs. old, army, 588747"
 "Kierstenesha Rolin (F), 30 yrs. old, army, 588747"
 "Kierstenesha Rolin (F), 34 yrs. old, army, 487747"
 "Kierstenesha Rolin (F), 38 yrs. old, army, 487747"
 "Kierstenesha Rolin (F), 42 yrs. old, army, 487747"
 "Kierstenesha Rolin (F), 46 yrs. old, army, 487747"
 "Kierstenesha Rolin (F), 50 yrs. old, army, 376747"
 "Kierstenesha Rolin (F), 54 yrs. old, army, 376747"
 "Kierstenesha Rolin (F), 58 yrs. old, army, 376747"
 "Kierstenesha Rolin (F), 62 yrs. old, army, 376747"
 "Kierstenesha Rolin (F), 66 yrs. old, army, 356647"
 "Kierstenesha Rolin (F), 70 yrs. old, army, 356647"
 "Kierstenesha Rolin (F), 74 yrs. old, army, 356647"
 "Kierstenesha Rolin (F), 78 yrs. old, army, 356647"
 "Kierstenesha Rolin (F), 82 yrs. old, army, 356647"
 "Kierstenesha Rolin (F), 86 yrs. old, army, 356647"
 "Kierstenesha Rolin (F), 90 yrs. old, army, 356647"
 "Kierstenesha Rolin (F), 94 yrs. old, army, 356647"]


;; Death from old age:
(->> (starting-character)
     (iterate age-one-term)
     (take 20)
     last)

;;=>
{:royal-form nil,
 :reinlisting? true,
 :actual-service :navy,
 :generation nil,
 :age 94,
 :commissioned? false,
 :living? false,
 :rank 0,
 :first-name "Eika",
 :surnames '("Herman" "Meehan"),
 :prefix "Ms.",
 :drafted? true,
 :rank-name nil,
 :desired-service :marines,
 :gender :female,
 :attributes {:ss 4, :ed 7, :in 9, :en 0, :dx 1, :st 1}}


;; Skills for characters:
(->> make-character
     (repeatedly 20)
     (map (juxt format-name-map (comp vec :skills)))
     vec)

;;=>
[["Ynthia Hienz (F), 26 yrs. old, marines, 586955"
  ["Mechanical" "ATV" "Electronic"]]
 ["Lema (F), 38 yrs. old, 578C9B"
  ["Mechanical"
   "Streetwise"
   "Electronic"
   "Forgery"
   "Electronic"
   "Brawling"]]
 ["Cepcion Cques (F), 22 yrs. old, army, 99C885"
  ["Mechanical" "GunCbt"]]
 ["Lieutenant Riel Eather (M), 22 yrs. old, army, A6687A"
  ["Electronic" "Mechanical"]]
 ["Ms. Cathenika Erre Rahul Kenneth (F), 22 yrs. old, army, B8965A"
  ["GunCbt" "ATV"]]
 ["FourthOffc Eetta (F), 26 yrs. old, merchant, 98CA86"
  ["Navigation" "Streetwise" "Mechanical"]]
 ["Ms. Laila (F), 26 yrs. old, 5B8529"
  ["Streetwise" "Forgery" "Streetwise"]]
 ["SecndOffc Ncey Polly Lsky Amartin Ecky (F), 38 yrs. old, merchant, 787898"
  ["Electronic"
   "Electronic"
   "Navigation"
   "Electronic"
   "Streetwise"
   "Mechanical"]]
 ["Ertude Arkus Offrey Lliot (F), 34 yrs. old, marines, 576588"
  ["BladeCbt" "Mechanical" "GunCbt" "GunCbt" "Mechanical"]]
 ["Lieutenant Mara Glas (F), 22 yrs. old, army, 959477"
  ["Tactics" "Mechanical"]]
 ["FourthOffc Jeffery Magnus (M), 34 yrs. old, merchant, 9B6625"
  ["Streetwise" "Medical" "Medical" "Medical" "Electronic"]]
 ["FourthOffc Rnestor Ikolai (M), 34 yrs. old, merchant, 666357"
  ["Navigation" "Mechanical" "Streetwise" "Navigation" "Gunnery"]]
 ["Srta. Nilda Etin (F), 26 yrs. old, scouts, 896A37"
  ["Mechanical" "Jack-o-T" "Mechanical"]]
 ["Ayton Eddy Icia (M), 26 yrs. old, scouts, 288445"
  ["AirRaft" "Medical" "Medical"]]
 ["M. Ucas Cois Ella Icah Kemal (M), 26 yrs. old, marines, A67886"
  ["Tactics" "BladeCbt" "Mechanical"]]
 ["FourthOffc Iuseppe Hmed Xana Debi Serdar, III (M), 26 yrs. old, merchant, C82437"
  ["Gunnery" "Navigation" "Gunnery"]]
 ["Emona Seph Hleen Muel (F), 22 yrs. old, 867948"
  ["Brawling" "Electronic"]]
 ["Milford Wson (M), 30 yrs. old, scouts, 894B6A"
  ["Medical" "Mechanical" "Electronic" "Electronic"]]
 ["Blair Urtney (F), 26 yrs. old, scouts, 6585A3"
  ["Jack-o-T" "AirRaft" "Medical"]]
 ["Colonel Temberyle Ulius Uise Elijah Ahesh (F), 38 yrs. old, army, 7479C7"
  ["ATV" "Electronic" "Electronic" "ATV" "Mechanical" "Electronic"]]]


