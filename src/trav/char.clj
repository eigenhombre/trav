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


(defn specialize [skill]
  (condp = skill
    'BladeCbt (rand-nth blades)
    'GunCbt (rand-nth guns)
    skill))


(defn add-actual-skill [char skill]
  (if-let [{attr :attr, delta :delta} (attribute-change skill)]
    (update-in char [:attributes attr] + delta)
    (update-in char [:skills (specialize skill)] (fnil inc 0))))


(defn add-skill [{:keys [actual-service living?] :as char}]
  (if-not living?
    char
    (let [skill (-> (select-skill-table char) actual-service rand-nth)]
      (add-actual-skill char skill))))


(defn add-skills-for-service-term
  "
  First term yields two skills; subsequent terms yield one.
  "
  [{:keys [terms-reached] :as char}]
  (if (= terms-reached 1)
    (-> char add-skill add-skill)
    (-> char add-skill)))


(defn add-automatic-skills-for-rank [{service :actual-service
                                      rank-name :rank-name
                                      :as char}]
  (cond
   (and (= service :navy) (= rank-name "Captain")) (add-actual-skill char 'SS+1)
   (and (= service :navy) (= rank-name "Admiral")) (add-actual-skill char 'SS+1)

   (and (= service :marines) (= rank-name "Lieutenant"))
   (add-actual-skill char 'Revolver)

   (and (= service :army) (= rank-name "Lieutenant"))
   (add-actual-skill char 'SMG)

   (and (= service :merchant) (= rank-name "FirstOffc"))
   (add-actual-skill char 'Pilot)

   :else char))


(defn add-automatic-skills [{service :actual-service :as char}]
  (condp = service
    :marines (add-actual-skill char 'Cutlass)
    :army (add-actual-skill char 'Rifle)
    :scouts (add-actual-skill char 'Pilot)
    char))


(defn maybe-increase-rank [char]
  (let [rank-vals (vec (ranks (:actual-service char)))
        rank-name (get rank-vals (:rank char))]
    (if rank-name
      (-> char
          (update-in [:rank] inc)
          (assoc :rank-name (str rank-name))
          add-automatic-skills-for-rank
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


(defn muster-out
  "
  One benefit roll per term; one more if rank 1-2; two more if rank >
  2.  cash-allowances table can be used at most three times.
  Optional(?) DM of +1 on material-benefits table if rank > 4.
  Characters with gambling skill get +1 on cash-allowances table.
  "
  [{rank :rank
    terms :terms-reached
    alive :living?
    {gambling 'Gambling} :skills
    :as char}]
  (if-not alive
    char
    (let [rolls (cond (= rank 0) terms
                      (< rank 3) (inc terms)
                      :else (+ 2 terms))]
      ;; (println 'musterout rank terms rolls gambling) <--- YAH
      char)))


(defn make-character []
  (->> (starting-character)
       add-automatic-skills
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
     (remove (complement :living?))  ;; Allow survivors only!
     (sort-by :name)
     (map (juxt format-name-map skills-str))
     vec)


;;=>
[["Rueben Kolai Lner (M), 42 yrs. old, scouts, 9B8944"
  "Jack-o-T-1, Mechanical-1, AirRaft-1, Medical-2, Pilot-1"]
 ["Miss Ante Rotoshi Irofumi Ille Elric (F), 34 yrs. old, 9C55A6"
  "Forgery-1, Bayonet-1, Broadsword-1, Medical-1"]
 ["FourthOffc Sir Rardo Aoto, I (M), 30 yrs. old, merchant, 5A696B"
  "Electronic-1, Streetwise-1, Pike-1"]
 ["Hantana Colm (F), 22 yrs. old, navy, 979577" "AutomaticPistol-1"]
 ["Sir Danela Heal Ariou Inda Varda (F), 26 yrs. old, marines, 688B8B"
  "Brawling-2, Tactics-1, Cutlass-1"]
 ["Lympia Tapwant Rodger Aura, 26 yrs. old, 676668"
  "Gambling-1, Brawling-1, LaserCarbine-1"]
 ["FourthOffc Selleena Panacea Ender Nivas Vilhelm (F), 22 yrs. old,
   merchant, 7C7658"
  "Brawling-1, Medical-1, Electronic-1"]
 ["Ms. Keilah Amin (F), 26 yrs. old, navy, 997675"
  "Broadsword-1, Mechanical-1, VaccSuit-1"]
 ["Vito Ewis Artin, III (M), 26 yrs. old, scouts, 597984"
  "Navigation-1, Medical-1, Pilot-1"]
 ["FourthOffc Ernon Samuel (M), 26 yrs. old, merchant, 434887"
  "Computer-1, Electronic-1, Medical-1"]
 ["Carmine Rtney (M), 26 yrs. old, 62D346" "Bribery-1, LaserCarbine-1"]
 ["Rard An-chris Very (M), 22 yrs. old, 4B5478"
  "Gambling-1, Halberd-1"]
 ["Srta. Linda Tair (F), 26 yrs. old, 8746A5"
  "Broadsword-1, Brawling-1, Forgery-1"]
 ["Nnemarylyn Rilyn Dewey (F), 22 yrs. old, 967965" "Brawling-1"]
 ["Lieutenant Danilo Mour Bonnie (M), 22 yrs. old, army, D78968"
  "SMG-1, Sword-1, Rifle-2"]
 ["Rcos Iton (M), 34 yrs. old, 6A7499"
  "Electronic-1, Forgery-1, Brawling-1, Bribery-2"]
 ["Mr. Von Toine Suwandip Dward Hnnie Lcolm, Jr. (M), 26 yrs. old, 74866C"
  "Electronic-1, Rifle-1, Brawling-1"]
 ["Ms. Erry Belindsay Ucky Anjay Bbie (F), 22 yrs. old, marines, 87A245"
  "SMG-1, Cutlass-1"]
 ["Sir Ncee Laura (F), 30 yrs. old, 6B8C6B" "Brawling-3"]
 ["Mr. Von Ctor Orvillermo Aola, I (M), 22 yrs. old, 33736B"
  "Brawling-1"]
 ["Ms. Phylissia Rdar (F), 26 yrs. old, 58A9A8"
  "Gambling-1, Jack-o-T-1, Brawling-1"]
 ["Mr. Elson Ztof (M), 22 yrs. old, 698724" "Brawling-1"]
 ["Edwin Hawn Nora (M), 22 yrs. old, merchant, 29CA78"
  "Mechanical-1, Electronic-1"]
 ["FourthOffc Itlyn (F), 26 yrs. old, merchant, A238CC"
  "Cutlass-1, Admin-1"]
 ["Ms. Asilia Anielle (F), 22 yrs. old, marines, 487858"
  "LaserCarbine-1, ATV-1, Cutlass-1"]
 ["Rrance Barry (M), 42 yrs. old, scouts, 579877"
  "Gunnery-2, AirRaft-1, Medical-2, Pilot-1"]
 ["FourthOffc Lnora (F), 42 yrs. old, merchant, AAD577"
  "Sword-1, Broadsword-1, Mechanical-1, VaccSuit-1, Brawling-1"]
 ["Fr. Ikaela Ovan (F), 22 yrs. old, scouts, 3B88A5"
  "AirRaft-1, Pilot-1"]
 ["Captain Marling Hatter, V (M), 30 yrs. old, army, B25786"
  "AirRaft-1, Gambling-1, SMG-1, ATV-1, Mechanical-1, Rifle-1"]
 ["Maxima Nkar Jarving Iping Arten (F), 26 yrs. old, 35997A"
  "Halberd-1, Blade-1, Electronic-1"]
 ["FourthOffc Trinidad Mandal, Jr. (M), 22 yrs. old, merchant, 4866B3"
  "Brawling-1, Medical-1, Blade-1, Pilot-1"]
 ["Ndolynella Kristen (F), 22 yrs. old, navy, A95958" "Electronic-1"]
 ["Usty Ndie (M), 22 yrs. old, merchant, 76C3A8" "Medical-1"]
 ["FourthOffc Vora Artmann (F), 30 yrs. old, merchant, 7B6766"
  "Jack-o-T-1, Electronic-1, Streetwise-1"]
 ["FourthOffc Ayden Gideon Floyd Ladislaw Harryl, Jr. (M), 22 yrs. old,
   merchant, B928A2"
  "Jack-o-T-1, Medical-2, Pilot-1"]
 ["Lieutenant Janie Rine Tair (F), 26 yrs. old, army, 778742"
  "Mechanical-1, AutomaticPistol-1, SMG-1, ATV-1, Rifle-1"]
 ["FourthOffc Espina (F), 34 yrs. old, merchant, 5B1927"
  "Gunnery-1, Mechanical-1, Steward-1, Navigation-1, Electronic-3"]]



(->> make-character
     (repeatedly 5000)
     (remove (complement :living?))  ;; Allow survivors only!
     (filter #(= (:actual-service %) :army))
     (filter #(= (:rank-name %) "Lieutenant"))
     (take 10)
     (map (juxt format-name-map skills-str))
     vec)

;;=>  As according to the "Rank and Service Skills" table, they all have SMG:
[["Lieutenant Ximo Oseph Cynthias Vijay, III (M), 22 yrs. old, army, 3A63A5"
  "Medical-1, SMG-1, Mechanical-1, BladeCbt-1, Rifle-1"]
 ["Lieutenant Miesha Rtney Iane Wson (F), 26 yrs. old, army, A766A2"
  "Gambling-1, Medical-1, SMG-1, Tactics-1, Admin-1, Leader-1, Rifle-1"]
 ["Lieutenant Llettina Lenn (F), 22 yrs. old, army, 58A8CA"
  "SMG-1, Tactics-1, Mechanical-1, Admin-1, Rifle-1"]
 ["Lieutenant Armida Aoto Boyce Hadow (F), 34 yrs. old, army, 598B7A"
  "Brawling-1, Mechanical-2, SMG-1, Electronic-1, Tactics-1, BladeCbt-1,
   Rifle-1"]
 ["Lieutenant Ique Hellen Ijah (F), 22 yrs. old, army, 4A77A4"
  "GunCbt-1, SMG-1, BladeCbt-3, Rifle-1"]
 ["Lieutenant Kinley Jordan (M), 22 yrs. old, army, 8A777A"
  "BladeCbt-1, SMG-1, GunCbt-1, Rifle-1"]
 ["Lieutenant Hyun Briggs Uashi (F), 22 yrs. old, army, 997744"
  "SMG-1, BladeCbt-1, Gambling-2, Rifle-1"]
 ["Lieutenant Raymundo Kathrin, Sr. (M), 22 yrs. old, army, AC69BA"
  "Brawling-1, SMG-1, BladeCbt-1, Rifle-1"]
 ["Lieutenant Von Acey Lila (M), 26 yrs. old, army, 39789C"
  "GunCbt-2, SMG-1, Medical-1, Leader-2, Rifle-1"]
 ["Lieutenant Brooks Armi (M), 22 yrs. old, army, 8A9A63"
  "SMG-1, FwdObsv-1, GunCbt-2, Mechanical-1, Rifle-1"]]
