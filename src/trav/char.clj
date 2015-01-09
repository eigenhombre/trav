(ns trav.char
  (:require [trav.dice :refer [d]]
            [trav.util :refer [hexcode take-until keywordize]]
            [trav.tables :refer :all]
            [namejen.names :refer [gen-name-data-as-map]]))


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


(defn upp [char]
  (->> char
       :attributes
       (#(map % attributes))
       (map hexcode)
       (apply str)))


(defn skills-str [{skills :skills}]
  (->> skills
       (map (fn [[a b]] (format "%s-%s" a b)))
       (interpose ", ")
       (apply str)))


(defn as-syms [s] (vec (map symbol (clojure.string/split s #" "))))


(defn roll-for-service-table-succeeds? [table char]
  (let [stats (:attributes char)
        {:keys [base-roll dms]} (->> char
                                     :actual-service
                                     (#(table %)))]
    (roll-with-dms-succeeds? base-roll dms stats)))


;; Terms of service
(defn select-skill-table [{{ed :ed} :attributes}]
  (->> [personal-development-table
        service-skills-table
        advanced-education-table
        advanced-education-table-2]
       (take (if (>= ed 8) 4 3))
       rand-nth))


(defn attribute-change
  "
  Handle DX+1 or SS-1 type 'skills', yielding a map when given that
  sort of symbol, otherwise nil.
  "
  [sym]
  (if-let [[_ sattr snum] (->> sym name (re-find #"(\w{2})([+-]\d+)"))]
    {:attr (keywordize sattr)
     :delta (Integer. snum)}))


(defn add-skill [{:keys [actual-service living?] :as char}]
  (if-not living?
    char
    (let [skill (-> (select-skill-table char) actual-service rand-nth)]
      (if-let [{attr :attr, delta :delta} (attribute-change skill)]
        (update-in char [:attributes attr] + delta)
        (update-in char [:skills skill] (fnil inc 0))))))


(defn add-skills-for-service-term
  "
  First term yields two skills; subsequent terms yield one.
  "
  [{:keys [terms-reached] :as char}]
  (if (= terms-reached 1)
    (-> char add-skill add-skill)
    (-> char add-skill)))


(defn maybe-increase-rank [char]
  (let [rank-vals (vec (ranks (:actual-service char)))
        rank-name (get rank-vals (:rank char))]
    (if rank-name
      (-> char
          (update-in [:rank] inc)
          (assoc :rank-name (str rank-name))
          add-skill)
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
               add-skill
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


;; YAH: mustering out benefits
(defn muster-out [char] char)


(defn make-character []
  (->> (starting-character)
       (iterate apply-term-of-service)
       (take-until (fn [m] (or (not (:reinlisting? m))
                               (not (:living? m)))))
       last
       muster-out))


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
     (map (juxt format-name-map skills-str))
     vec)

;;=>
[["FourthOffc Rryl Ymour Taurus, Sr. (M), 34 yrs. old, merchant, 5A76C2"
  "Brawling-1, Jack-o-T-1, Navigation-1, Admin-2, VaccSuit-1"]
 ["FourthOffc Taisha Olson (F), 22 yrs. old, merchant, A35998"
  "Admin-1, Navigation-1, Medical-2"]
 ["Herr Odricky (M), 22 yrs. old, navy, B96644" "VaccSuit-1"]
 ["Captain Sir Adley Dford, III (M), 26 yrs. old, army, 6656CB"
  "Leader-1, GunCbt-1, Tactics-2, Brawling-1"]
 ["Lyson Liza (F), 22 yrs. old, 458B3A" "Electronic-1"]
 ["Sanford Ahul Jarsh Tran Nolis (M), 22 yrs. old, marines, 887988"
  "Medical-1, Admin-1"]
 ["Ms. Indsy Ulius (F), 22 yrs. old, marines, A68373"
  "VaccSuit-1, GunCbt-1"]
 ["Fr. Aticia Uyuki Arten (F), 22 yrs. old, 5AC4B6" "Gambling-1"]
 ["FourthOffc Eopoldo Sanan (M), 26 yrs. old, merchant, 893949"
  "Navigation-1, Jack-o-T-1, Steward-1, VaccSuit-1"]
 ["FourthOffc Millard Alexis (M), 26 yrs. old, merchant, 8A9464"
  "Gunnery-1, Medical-1, Brawling-1"]
 ["Ergio (M), 22 yrs. old, navy, 987879" "BladeCbt-2"]
 ["Ms. Leslie Orothy Piotr Hard (F), 30 yrs. old, 578673"
  "Brawling-1, Gambling-1"]
 ["Ms. Dath Oderick (F), 22 yrs. old, scouts, 2A8876"
  "Mechanical-1, Electronic-1"]
 ["FourthOffc Donita (F), 22 yrs. old, merchant, D75878"
  "BladeCbt-1, VaccSuit-1, Electronic-1"]
 ["FourthOffc Margarito Edro Etsy (M), 30 yrs. old, merchant, 89695A"
  "Steward-1, Gunnery-1, Navigation-1, BladeCbt-1, Electronic-1, Medical-1"]
 ["Sir Inton Osur Jenine Swamy Usan (M), 34 yrs. old, 547A73"
  "Mechanical-1, Gambling-2, BladeCbt-2"]
 ["Ernanderely Aphyllos (F), 26 yrs. old, navy, 989657"
  "VaccSuit-1, Mechanical-1"]
 ["Lieutenant Lfonzo Rayant, I (M), 22 yrs. old, army, 697C94"
  "Medical-1, Admin-1, GunCbt-1, BladeCbt-1"]
 ["Morton Endi, III (M), 22 yrs. old, navy, 783369"
  "Jack-o-T-1, Electronic-1"]
 ["FourthOffc Pher Ewis Aphyllos Iroze, Jr. (M), 42 yrs. old, merchant, B458A5"
  "Admin-1, Engnrng-1, Medical-1, Brawling-1, Gunnery-1"]
 ["Mr. Rron Einhard (M), 22 yrs. old, 668736" "Gambling-1, Forgery-1"]
 ["Ms. Atarshala Andip Dustin Cisco Klin (F), 30 yrs. old, 489468"
  "Bribery-1, Gambling-1, Mechanical-1"]
 ["Sra. Ueen Erta (F), 22 yrs. old, scouts, A9C667" "Mechanical-1"]
 ["Fermin Norma (M), 42 yrs. old, A64B67"
  "Electronic-2, Streetwise-1, Mechanical-1"]
 ["Lieutenant Orna Evyn (F), 22 yrs. old, army, 877997"
  "Electronic-1, ATV-1, BladeCbt-1"]
 ["Sir Nastaciela Anaka (F), 22 yrs. old, 65988B" "Brawling-1"]
 ["Captain Craig (M), 26 yrs. old, army, B58689"
  "Mechanical-1, BladeCbt-1, GunCbt-1, ATV-1, FwdObsv-1, Medical-1"]
 ["Lieutenant Arlton Itendranatolerant (M), 22 yrs. old, marines, 525868"
  "BladeCbt-2, GunCbt-2"]
 ["SecndOffc Karee Dent (F), 58 yrs. old, merchant, C78A75"
  "VaccSuit-1, Electronic-2, Medical-1, Navigation-1, Mechanical-4"]
 ["FourthOffc Ymour (M), 26 yrs. old, merchant, 599357"
  "Gunnery-1, Steward-1, Electronic-3"]
 ["Cole Olai (M), 22 yrs. old, scouts, 953B33"
  "VaccSuit-1, Jack-o-T-1"]
 ["Lieutenant Hunter Patty (M), 22 yrs. old, army, 8586B5"
  "Tactics-1, ATV-1, GunCbt-1"]
 ["Dwayne Icah (M), 26 yrs. old, scouts, 558A96"
  "Electronic-1, Jack-o-T-1"]
 ["FourthOffc Amado Ollin Matti Assan (M), 30 yrs. old, merchant, 989877"
  "Streetwise-1, GunCbt-1, Electronic-1, Navigation-1"]
 ["Zell Nadeep Susumu Spudboy Torianne (F), 22 yrs. old, scouts, 729B65"
  ""]
 ["Arkus Arryl (M), 22 yrs. old, army, 8CC9A9" "AirRaft-1"]
 ["Tzie Omain (F), 22 yrs. old, 638956" "GunCbt-1, Bribery-1"]
 ["Ronnie Leste Erant, V (M), 22 yrs. old, navy, 589456" "VaccSuit-1"]
 ["Major Hilma Icky (F), 34 yrs. old, army, 977B77"
  "Brawling-1, AirRaft-1, Tactics-1, Mechanical-1, Gambling-1, BladeCbt-2"]]


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
