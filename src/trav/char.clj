(ns trav.char
  "
  This is the character generation namespace.  Basically it implements
  the rules described in the first half of Classic Traveller book 1.
  In most cases where the player would have a choice, uniform random
  selections are taken.
  "
  (:require [trav.dice :refer [d]]
            [trav.util :refer [hexish-code take-until keywordize]]
            [trav.tables :refer :all]
            [namejen.names :refer [gen-name-data-as-map]]))


(defn roll-with-stats-dms-succeeds? [base-roll dms stats]
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
        success? (roll-with-stats-dms-succeeds?
                  (-> enlistment desired-service :base-roll)
                  service-dms
                  stats)
        actual-service (if success? desired-service (rand-nth services))]
    {:desired-service desired-service
     :drafted? (not success?)
     :actual-service actual-service}))


(defn char-attr-map
  "
  Generate random character attributes (ST, DX, etc.) using 2D each.
  "
  []
  (->> d
       repeatedly
       (take (count attributes))
       (zipmap attributes)))


(defn starting-character
  "
  Generate starting character prior to terms of service, using
  Namejen-provided map.  Select desired and actual service based on
  dice throws and the 'draft.'
  "
  []
  (let [stats (char-attr-map)
        soc (get stats :ss)
        knighted? (= soc 11)
        baron? (= soc 12)
        royal-form (cond knighted? "Sir"
                         baron? "Von")]
    (merge (gen-name-data-as-map)
           (determine-service stats)
           {:age 18
            :royal-form royal-form
            :attributes stats
            :living? true
            :commissioned? false
            :reinlisting? true
            :credits 0
            :possessions []
            :rank 0
            :memberships #{}
            :rank-name nil})))


;; Examples:
(->> starting-character
     repeatedly
     (take 3)
     vec)

;;=>
[{:royal-form nil,
  :reinlisting? true,
  :actual-service :marines,
  :generation nil,
  :age 18,
  :commissioned? false,
  :living? true,
  :rank 0,
  :first-name "Lucindy",
  :surnames ["Llary" "Socorris"],
  :prefix "Ms.",
  :drafted? false,
  :rank-name nil,
  :desired-service :marines,
  :gender :female,
  :attributes {:ss 7, :ed 11, :in 8, :en 6, :dx 7, :st 6}}
 {:royal-form nil,
  :reinlisting? true,
  :actual-service :navy,
  :generation nil,
  :age 18,
  :commissioned? false,
  :living? true,
  :rank 0,
  :first-name "Kasey",
  :surnames ["Celias" "Vishall"],
  :prefix nil,
  :drafted? false,
  :rank-name nil,
  :desired-service :navy,
  :gender :female,
  :attributes {:ss 9, :ed 9, :in 8, :en 8, :dx 4, :st 4}}
 {:royal-form "Von",
  :reinlisting? true,
  :actual-service :other,
  :generation nil,
  :age 18,
  :commissioned? false,
  :living? true,
  :rank 0,
  :first-name "Estrell",
  :surnames ["Raphael" "Lhelm"],
  :prefix "Ms.",
  :drafted? true,
  :rank-name nil,
  :desired-service :navy,
  :gender :female,
  :attributes {:ss 12, :ed 4, :in 6, :en 7, :dx 5, :st 7}}]




(defn upp [char]
  (->> char
       :attributes
       (#(map % attributes))
       (map hexish-code)
       (apply str)))


(defn format-skills [{skills :skills}]
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
    (roll-with-stats-dms-succeeds? base-roll dms stats)))


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


(defn select-skill-table [{{ed :ed} :attributes}]
  (->> [personal-development-table
        service-skills-table
        advanced-education-table
        advanced-education-table-2]
       (take (if (>= ed 8) 4 3))
       rand-nth))


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


(defn- specify-blade [char possession]
  (if (= possession 'Blade)
    (let [skills (keys (:skills char))
          match (some (set skills) blades)]
      (if match
        match
        (rand-nth blades)))
    possession))


(defn- specify-gun [char possession]
  (if (= possession 'Gun)
    (let [skills (keys (:skills char))
          match (some (set skills) guns)]
      (if match
        match
        (rand-nth guns)))
    possession))


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
    service :actual-service
    {gambling 'Gambling} :skills
    :as char}]
  (if-not alive
    char
    (let [rolls (cond (= rank 0) terms
                      (< rank 3) (inc terms)
                      :else (+ 2 terms))
          cash-rolls (->> rolls
                          inc
                          rand-int
                          (min 3))
          cash-lookup-fn #(if gambling (d 1) (dec (d 1)))
          cash-fn #(-> cash-allowances service (nth (cash-lookup-fn)))
          cash (apply + (repeatedly cash-rolls cash-fn))

          benefit-fn (fn []
                       ;; Simulate choice of +1 DM for high-rank by increasing
                       ;; range of throws:
                       (let [throw-range (if (> rank 4) 7 6)
                             idx (rand-int throw-range)]
                         (-> material-benefits
                             service
                             (nth idx))))
          noncash-rolls (- rolls cash-rolls)
          benefits (remove nil? (repeatedly noncash-rolls benefit-fn))
          travellers (some #{'Travellers} benefits)
          possessions (->> benefits
                           (remove attribute-change)
                           (remove #{'Travellers})
                           (map (partial specify-blade char))
                           (map (partial specify-gun char)))
          skills (filter attribute-change benefits)
          improved-char (reduce add-actual-skill char skills)]
      (-> improved-char
          (update-in [:credits] + cash)
          (#(if travellers (update-in % [:memberships] conj 'Travellers) %))
          (update-in [:possessions] concat possessions)))))


(defn make-character []
  (->> (starting-character)
       add-automatic-skills
       (iterate apply-term-of-service)
       (take-until (fn [m] (or (not (:reinlisting? m))
                               (not (:living? m)))))
       last
       muster-out))


(defn make-living-character []
  (->> make-character
       (repeatedly)
       (remove (complement :living?))  ;; Allow survivors only!
       first))


(defn format-swag [{:keys [possessions
                           memberships
                           credits]}]
  (let [fmt-seq #(->> %
                      (interpose ", ")
                      (apply str))]
    (fmt-seq (concat (sort possessions)
                     memberships
                     [(format "%d CR" credits)]))))


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
(->> make-living-character
     (repeatedly 50)
     (sort-by :name)
     (map (juxt format-name-map format-skills format-swag))
     vec)

;;=>
[["Rgeannemaryrosendall Heatherine Erat (F), 26 yrs. old, 6899A8"
  "Forgery-1, Cudgel-1, AutomaticPistol-1"
  "LowPsg, 10000 CR"]
 ["Euben Orvillermo (M), 30 yrs. old, 78DB96"
  "Spear-1, Forgery-1, Gambling-1"
  "70000 CR"]
 ["Leonel Takeuchi Areq Samuel (M), 22 yrs. old, scouts, 876256"
  "Mechanical-1, Pilot-1"
  "20000 CR"]
 ["Chitley Dboy (F), 30 yrs. old, 297692"
  "Bribery-1, Electronic-2, Streetwise-1"
  "30000 CR"]
 ["Bess (F), 22 yrs. old, army, 795495"
  "Carbine-1, Electronic-1, Rifle-1"
  "5000 CR"]
 ["LtColonel Eron Ernie Liff (M), 38 yrs. old, army, A55AF6"
  "AirRaft-1, Bayonet-1, Gambling-2, Mechanical-1, AutomaticPistol-1, FwdObsv-1,
   Rifle-1, Broadsword-1, ATV-1, SMG-1"
  "AutomaticPistol, LowPsg, 45000 CR"]
 ["Danyel Erly (F), 26 yrs. old, 64A674"
  "Brawling-1, Gambling-1"
  "LaserCarbine, 0 CR"]
 ["Ympia Ford Anuel (F), 22 yrs. old, navy, 38B79A"
  "FwdObsv-1"
  "Spear, 0 CR"]
 ["FourthOffc Irio Rtmann (M), 30 yrs. old, merchant, A557A6"
  "Pilot-1, Navigation-2, Medical-2, Admin-1"
  "LowPsg, LowPsg, Pike, 0 CR"]
 ["Dmund Oria Ckye Ncan (M), 22 yrs. old, 65B982"
  "Forgery-1, Brawling-1"
  "10000 CR"]
 ["Lieutenant Fugio Gela Udge (M), 26 yrs. old, marines, 77B644"
  "SMG-1, Revolver-1, AutomaticRifle-2, ATV-1, Gambling-1, Cutlass-1"
  "65000 CR"]
 ["FourthOffc Nklyn Aron Yvonne Rafael (M), 26 yrs. old, merchant, D5B746"
  "Electronic-1, Medical-2"
  "Cudgel, Cutlass, LowPsg, 0 CR"]
 ["Mr. Filibert Njeri (M), 22 yrs. old, navy, 767947"
  "Electronic-1"
  "1000 CR"]
 ["Chiquita Joyce (F), 22 yrs. old, army, 683779"
  "AutomaticRifle-1, ATV-1, Rifle-1"
  "2000 CR"]
 ["Lieutenant Vickey Torianto (F), 22 yrs. old, army, 99A288"
  "Leader-1, SMG-1, FwdObsv-1, AutomaticRifle-1, Rifle-1"
  "12000 CR"]
 ["FourthOffc Rubin Achim, V (M), 34 yrs. old, merchant, 546B76"
  "Cudgel-1, BodyPistol-1, Medical-1, Mechanical-1, Navigation-1"
  "Cudgel, 40000 CR"]
 ["Lieutenant Radley Exis, Jr. (M), 34 yrs. old, navy, AA5868"
  "LaserCarbine-1, VaccSuit-1, Electronic-1, Blade-1, FwdObsv-2"
  "LowPsg, Travellers, 5000 CR"]
 ["FourthOffc Rmelo Onstantin Ranathan Udith Iellen (M), 22 yrs. old, merchant,
   84C587"
  "Engnrng-1, Streetwise-1, Navigation-1, Bayonet-1"
  "30000 CR"]
 ["Orvillermo Nter (M), 34 yrs. old, marines, 3A6C69"
  "ATV-2, Blade-1, Tactics-1, Cutlass-1"
  "HighPsg, 25000 CR"]
 ["Pandon (F), 26 yrs. old, 86BA78" "Brawling-1, Forgery-1" "10000 CR"]
 ["Lieutenant Arcus (M), 22 yrs. old, army, 873AC6"
  "LaserRifle-2, SMG-2, Rifle-1"
  "20000 CR"]
 ["ThirdOffc Ylin Pradek (F), 38 yrs. old, merchant, A668C7"
  "Electronic-1, SMG-1, Steward-1, Mechanical-1, Engnrng-1"
  "LowPsg, LowPsg, SMG, 60000 CR"]
 ["Sario Lerant, Jr. (M), 26 yrs. old, marines, 63B6A9"
  "Computer-1, Tactics-2, Cutlass-1"
  "Cutlass, 0 CR"]
 ["FourthOffc Sir Ichie Artyn Tofer Gene Nigel (M), 22 yrs. old, merchant,
   A37B6B"
  "Jack-o-T-1, BodyPistol-1, Navigation-2"
  "50000 CR"]
 ["FourthOffc Ayle Spass Tyler Arda Iles, V (M), 22 yrs. old, merchant, 5BB756"
  "Mechanical-2, AutomaticRifle-1"
  "LowPsg, 20000 CR"]
 ["Ensign Colasandee Oklis (F), 38 yrs. old, navy, 2A87B8"
  "ShipsBoat-1, Broadsword-1, Pilot-1, FwdObsv-1, Jack-o-T-2, Admin-1"
  "Broadsword, HighPsg, 11000 CR"]
 ["FourthOffc Agueda Rabin Layton Xana Travi (F), 26 yrs. old, merchant, C88975"
  "BodyPistol-1, VaccSuit-1, Medical-1, Electronic-1"
  "BodyPistol, LowPsg, 5000 CR"]
 ["Mr. Monita Rleen Alter, 22 yrs. old, navy, 776847" "Spear-1" "0 CR"]
 ["Parker (M), 26 yrs. old, 675B48"
  "Cutlass-1, Brawling-1"
  "AutomaticRifle, Revolver, 0 CR"]
 ["Edgar (M), 22 yrs. old, 758963"
  "Streetwise-1, Forgery-1"
  "HighPsg, 0 CR"]
 ["Uline Rendranath Hsin Arie Belinder (F), 22 yrs. old, merchant, 595969"
  "Gunnery-1, Dagger-1"
  "Dagger, 0 CR"]
 ["FourthOffc Daryl (M), 26 yrs. old, merchant, 873693"
  "Medical-1, Electronic-1, Brawling-1"
  "Cudgel, LowPsg, 1000 CR"]
 ["Lieutenant Joslyn Leigh Scal Nkar Srael (F), 34 yrs. old, navy, 73689A"
  "Gunnery-1, SMG-1, Spear-1, FwdObsv-1"
  "LowPsg, Spear, Spear, Spear, 0 CR"]
 ["Mr. Lyle Dith (M), 22 yrs. old, marines, 5A9757"
  "Tactics-1, Foil-1, Cutlass-1"
  "0 CR"]
 ["FourthOffc Ulandorathyrn Icki (F), 26 yrs. old, merchant, 7B76A7"
  "Medical-1, Computer-1, Engnrng-1, Gunnery-1"
  "35000 CR"]
 ["Ensign Ashine Seenu (F), 26 yrs. old, navy, 598C85"
  "Engnrng-1, FwdObsv-1"
  "HighPsg, LowPsg, 20000 CR"]
 ["Lieutenant Sir Nolan Tinos Uashi Orey Vladim, I (M), 26 yrs. old, army,
   48572B"
  "Brawling-1, SMG-1, Mechanical-1, LaserRifle-1, Rifle-1"
  "30000 CR"]
 ["Lieutenant Von Svaldo Ratapwant Merat Udia (M), 26 yrs. old, navy, 9A776D"
  "Shotgun-1, Electronic-2"
  "HighPsg, HighPsg, 0 CR"]
 ["Mr. Ssac Land Ienz Sangho Marek, Jr. (M), 22 yrs. old, AA6364"
  "Brawling-1, Gambling-1"
  "100000 CR"]
 ["Udith Rgei (F), 26 yrs. old, 798A79"
  "Gambling-1, Brawling-1"
  "LowPsg, 50000 CR"]
 ["Ensign Daryl Lias, I (M), 26 yrs. old, navy, 7BB7B4"
  "Admin-1"
  "70000 CR"]
 ["FourthOffc Astasia Morrito (F), 26 yrs. old, merchant, 8558B3"
  "Admin-1, Sword-1, Steward-1, Gunnery-1"
  "LowPsg, 20000 CR"]
 ["Lieutenant Iseo Ssan (M), 22 yrs. old, army, 997BC7"
  "AutomaticPistol-1, SMG-1, Leader-1, Mechanical-1, Rifle-1"
  "30000 CR"]
 ["SecndOffc Erard (M), 34 yrs. old, merchant, 748934"
  "Electronic-1, Mechanical-1, Medical-1, Shotgun-1, VaccSuit-1, Jack-o-T-1,
   LaserRifle-1"
  "LaserRifle, LowPsg, LowPsg, 45000 CR"]
 ["Ance (M), 22 yrs. old, 366B94" "Pike-1" "0 CR"]
 ["Claren Chim Itty Mund Thryn (M), 34 yrs. old, scouts, 38A5BA"
  "Electronic-1, Gunnery-1, VaccSuit-2, Pilot-1"
  "70000 CR"]
 ["Sir Minnie Aiid Jerome (F), 22 yrs. old, marines, 7A3A7B"
  "Brawling-1, Mechanical-1, Cutlass-1"
  "20000 CR"]
 ["Rdie Want Ping Sylvan Imawan (F), 22 yrs. old, army, 43AB78"
  "Brawling-1, Gambling-1, Rifle-1"
  "30000 CR"]
 ["Lieutenant Maal Usannette (M), 22 yrs. old, army, 275AB8"
  "SMG-1, Mechanical-1, Electronic-1, Rifle-2"
  "0 CR"]
 ["Lieutenant Ugustino Hony (M), 26 yrs. old, marines, 345B83"
  "Revolver-1, Gambling-1, Admin-1, Computer-1, Tactics-2, Cutlass-1"
  "Cutlass, 0 CR"]]


;; Age distributions for living characters after service:
'(->> make-living-character
      (repeatedly 10000)
      (map :age)
      frequencies
      (map vec)
      (sort-by first)
      vec)
;;=>
[[22 4792]
 [26 2401]
 [30 1277]
 [34 681]
 [38 355]
 [42 207]
 [46 121]
 [50 68]
 [54 46]
 [58 23]
 [62 14]
 [66 9]
 [70 2]
 [74 1]
 [78 1]
 [82 1]
 [94 1]]
