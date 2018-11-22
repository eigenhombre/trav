(ns trav.chars
  "
  This is the character generation namespace.  Basically it implements
  the rules described in the first half of Classic Traveller book 1.
  In most cases where the player would have a choice, uniform random
  selections are taken.
  "
  (:require [trav.dice :refer [d]]
            [trav.util :refer [hexish-code take-until keywordize]]
            [trav.macros :refer :all]
            [namejen.names :refer [gen-name-data-as-map]]))


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


(def-rnd-selection-table personal-development-table
          navy   marines        army     scouts   merchant      other
  1       ST+1       ST+1       ST+1       ST+1       ST+1       ST+1
  2       DX+1       DX+1       DX+1       DX+1       DX+1       DX+1
  3       EN+1       EN+1       EN+1       EN+1       EN+1       EN+1
  4       SS+1   Gambling   Gambling     GunCbt       ST+1   BladeCbt
  5       IN+1   Brawling   Brawling       IN+1   BladeCbt   Brawling
  6       ED+1   BladeCbt       ED+1       ED+1   Brawling       SS-1)


(def-rnd-selection-table service-skills-table
          navy   marines        army     scouts   merchant      other
  1  ShipsBoat       ATV         ATV    AirRaft    Steward    Forgery
  2   VaccSuit  VaccSuit     AirRaft   VaccSuit   VaccSuit   Gambling
  3    FwdObsv  BladeCbt     FwdObsv Navigation       ST+1   Brawling
  4   BladeCbt  BladeCbt    BladeCbt Mechanical     GunCbt   BladeCbt
  5     GunCbt    GunCbt      GunCbt Electronic Electronic     GunCbt
  6    Gunnery    GunCbt      GunCbt   Jack-o-T   Jack-o-T    Bribery)


(def-rnd-selection-table advanced-education-table
          navy    marines       army     scouts   merchant      other
  1   VaccSuit        ATV        ATV    AirRaft Streetwise Streetwise
  2 Mechanical Mechanical Mechanical Mechanical Mechanical Mechanical
  3 Electronic Electronic Electronic Electronic Electronic Electronic
  4    Engnrng    Tactics    Tactics   Jack-o-T Navigation   Gambling
  5    Gunnery   BladeCbt   BladeCbt    Gunnery    Gunnery   Brawling
  6   Jack-o-T     GunCbt     GunCbt    Medical    Medical    Forgery)


(def-rnd-selection-table advanced-education-table-2
          navy    marines       army     scouts   merchant      other
  1    Medical    Medical    Medical    Medical    Medical    Medical
  2 Navigation    Tactics    Tactics Navigation Navigation    Forgery
  3    Engnrng    Tactics    Tactics    Engnrng    Engnrng Electronic
  4   Computer   Computer   Computer   Computer   Computer   Computer
  5      Pilot     Leader     Leader      Pilot      Pilot Streetwise
  6      Admin      Admin      Admin   Jack-o-T      Admin   Jack-o-T)


(def blades '(Dagger Blade Foil Cutlass Sword Broadsword
              Spear Halberd Pike Cudgel Bayonet))


(def guns '(BodyPistol AutomaticPistol Revolver Carbine Rifle
            LaserCarbine LaserRifle AutomaticRifle SMG Shotgun))


;; Mustering out / benefits tables
(def-rnd-selection-table material-benefits
          navy    marines       army     scouts   merchant      other
  1     LowPsg     LowPsg     LowPsg     LowPsg     LowPsg     LowPsg
  2       IN+1       IN+2       IN+1       IN+2       IN+1       IN+1
  3       ED+2       ED+1       ED+2       ED+2       ED+1       ED+1
  4      Blade      Blade        Gun      Blade        Gun        Gun
  5 Travellers Travellers    HighPsg        Gun      Blade    HighPsg
  6    HighPsg    HighPsg     MidPsg      Scout     LowPsg          -
  7       SS+2       SS+2       SS+1          -   Merchant          -)


(def-rnd-selection-table cash-allowances
          navy    marines       army     scouts   merchant      other
  1       1000       2000       2000      20000       1000       1000
  2       5000       5000       5000      20000       5000       5000
  3       5000       5000      10000      30000      10000      10000
  4      10000      10000      10000      30000      20000      10000
  5      20000      20000      10000      50000      20000      10000
  6      50000      30000      20000      50000      40000      50000
  7      50000      40000      30000      50000      40000     100000)


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
            :pension-annual 0
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
  (if (and (:living? char)
           (not (roll-for-service-table-succeeds? survival char)))
    (assoc char :living? false)
    char))


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


(defn- calc-pension [terms-served]
  (if (< terms-served 5)
    0
    (+ 2000 (* 2000 (- terms-served 4)))))


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
          (update-in [:possessions] concat possessions)
          (assoc :pension-annual (calc-pension terms))))))


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
                           pension-annual
                           credits]}]
  (let [fmt-seq #(->> %
                      (interpose ", ")
                      (apply str))]
    (fmt-seq (concat (sort possessions)
                     memberships
                     [(format "%d CR%s"
                              credits
                              (if (zero? pension-annual)
                                ""
                                (format " (%d CR annual)" pension-annual)))]))))


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
[["FourthOffc Brando Duane (F), 34 yrs. old, merchant, 76B966"
  "Cutlass-1, Electronic-1, Sword-2, Brawling-1"
  "Cutlass, 25000 CR"]
 ["Lieutenant Phoebe Itry Incenzo (F), 22 yrs. old, marines, 874333"
  "Revolver-2, ATV-1, Rifle-1, Cutlass-1"
  "Travellers, 5000 CR"]
 ["Nito Ongo Chuck (M), 22 yrs. old, 7A6275" "" "10000 CR"]
 ["Tephen Tibartfast (M), 26 yrs. old, navy, 6AA958"
  "VaccSuit-1"
  "LowPsg, LowPsg, 0 CR"]
 ["FourthOffc Arbie Ofumi (F), 22 yrs. old, merchant, 737B59"
  "Streetwise-1, Navigation-1, Brawling-1"
  "5000 CR"]
 ["Mual Hink (M), 22 yrs. old, marines, 6A9979"
  "LaserCarbine-1, Cutlass-1"
  "5000 CR"]
 ["Ms. Neomi Ajiv Gunnard Bias (F), 22 yrs. old, scouts, 7785A7"
  "Computer-1, Medical-1, Pilot-1"
  "30000 CR"]
 ["FourthOffc Von Usita Mike (F), 22 yrs. old, merchant, 89778C"
  "Spear-1, Jack-o-T-1"
  "LowPsg, 0 CR"]
 ["FourthOffc Lazaro Dharada (M), 26 yrs. old, merchant, C264B6"
  "Navigation-1, Engnrng-1, Steward-1, Brawling-1, Jack-o-T-1"
  "LaserRifle, 1000 CR"]
 ["Lieutenant Ergie (F), 22 yrs. old, army, 899365"
  "SMG-1, Brawling-1, AirRaft-1, Sword-1, Rifle-1"
  "HighPsg, 5000 CR"]
 ["Dison Liver Illy (M), 22 yrs. old, army, 4258A9"
  "Computer-1, AutomaticRifle-1, Rifle-1"
  "Rifle, 0 CR"]
 ["ThirdOffc Istie Mesh (F), 30 yrs. old, merchant, 672A6A"
  "Streetwise-1, Electronic-1"
  "Dagger, 42000 CR"]
 ["SecndOffc Oung Vadim Brette (M), 30 yrs. old, merchant, C93C82"
  "Admin-1, Computer-1, Medical-1, Engnrng-1, Electronic-2"
  "LowPsg, LowPsg, 70000 CR"]
 ["Huck Exandell (M), 22 yrs. old, army, 9A673A"
  "Mechanical-1, Shotgun-1, Rifle-1"
  "5000 CR"]
 ["FirstOffc Rrinn Rine Root (F), 38 yrs. old, merchant, 797A7A"
  "Electronic-1, Pilot-1, Revolver-1, Bayonet-1, Cudgel-1, Gunnery-2,
   VaccSuit-1, Medical-1"
  "Cudgel, Cudgel, Cudgel, 35000 CR (4000 CR annual)"]
 ["Vesteban Esan (M), 22 yrs. old, army, 499885"
  "ATV-1, Tactics-1, Rifle-1"
  "10000 CR"]
 ["Commander Miguel Sigurd (M), 34 yrs. old, navy, 858899"
  "FwdObsv-1, Gunnery-1, Engnrng-1, ShipsBoat-1, Pike-1, VaccSuit-1"
  "LowPsg, Pike, 50000 CR"]
 ["Captain Ovan Taurus (M), 26 yrs. old, army, BA3664"
  "ATV-1, AutomaticRifle-1, SMG-1, AutomaticPistol-1, Brawling-1, Rifle-1"
  "AutomaticPistol, HighPsg, 0 CR"]
 ["Dgar (M), 26 yrs. old, 9CA4A9"
  "Mechanical-1, AutomaticPistol-1, Brawling-1"
  "11000 CR"]
 ["FourthOffc Ssetta Rupert Darin Edith (F), 26 yrs. old, merchant, AB477A"
  "Jack-o-T-1, Electronic-1, Streetwise-1"
  "LaserCarbine, 10000 CR"]
 ["FourthOffc Von Lannie Moud Tuna Beckie (F), 26 yrs. old, merchant, 989A6C"
  "Electronic-1"
  "41000 CR"]
 ["Lenor Pilot Unter Moud Rooke (F), 38 yrs. old, 653979"
  "Mechanical-1, Brawling-1, Gambling-1, Forgery-1, Bayonet-1, Sword-1"
  "HighPsg, LaserCarbine, 55000 CR (4000 CR annual)"]
 ["Captain Troy Dolf Nudsen (M), 30 yrs. old, army, 844795"
  "Tactics-1, Medical-1, Brawling-1, SMG-1, FwdObsv-1, Admin-1, Revolver-1,
   Rifle-1"
  "HighPsg, 32000 CR"]
 ["Lieutenant Libert Huan (M), 42 yrs. old, marines, C456B9"
  "Revolver-1, Medical-1, VaccSuit-1, Tactics-1, Broadsword-1, ATV-3,
   Mechanical-1, Cutlass-1"
  "HighPsg, LowPsg, LowPsg, Travellers, 20000 CR (6000 CR annual)"]
 ["Lieutenant Rinidad Raig (M), 26 yrs. old, army, 66C7D2"
  "SMG-1, Brawling-1, Medical-1, Leader-1, Rifle-1"
  "MidPsg, 0 CR"]
 ["Ensign Leotilian Hamiltos (F), 22 yrs. old, navy, B87C89"
  "Admin-1, Medical-1"
  "LowPsg, 1000 CR"]
 ["M. Mickey Atolerant Neth (M), 30 yrs. old, navy, 67C654"
  "VaccSuit-1"
  "25000 CR"]
 ["Nora Rito Kevan Pilot Rley (F), 22 yrs. old, scouts, 848636"
  "VaccSuit-1, Gunnery-1, Pilot-1"
  "Cudgel, 0 CR"]
 ["Sir Rresa Mberly Roxane (F), 22 yrs. old, 5A798B"
  "AutomaticRifle-1, Streetwise-1"
  "1000 CR"]
 ["Eusebia Cather (F), 30 yrs. old, navy, 888F87"
  "FwdObsv-1"
  "HighPsg, 0 CR"]
 ["Ilson Iltos, I (M), 22 yrs. old, 658763" "" "50000 CR"]
 ["Ms. Osauran Frey Nice (F), 30 yrs. old, A5A657"
  "Forgery-1"
  "LowPsg, 55000 CR"]
 ["Lieutenant Errance Morgan Corey Jochen Metry (M), 22 yrs. old, army, 997488"
  "Brawling-1, SMG-1, Gambling-1, Electronic-1, Rifle-1"
  "20000 CR"]
 ["Mr. Intine Pper Lanny Rten, III (M), 26 yrs. old, army, 785774"
  "LaserRifle-1, ATV-1, Tactics-1, Rifle-1"
  "LowPsg, 10000 CR"]
 ["Ntionel Sanita (M), 34 yrs. old, scouts, 8B8995"
  "Jack-o-T-1, Navigation-1, AirRaft-1, Medical-2, Pilot-1"
  "Dagger, Sword, 30000 CR"]
 ["Morgan Rouk Arcel (M), 22 yrs. old, navy, 785B4A"
  "VaccSuit-1, SMG-1"
  "20000 CR"]
 ["Lieutenant Shea Plastair (F), 22 yrs. old, army, 9A65A7"
  "Tactics-1, SMG-1, BodyPistol-1, Leader-1, Rifle-1"
  "12000 CR"]
 ["Lieutenant Isiah Chim Debi Ardo (M), 22 yrs. old, army, 83A3BA"
  "Admin-1, SMG-1, Gambling-1, Electronic-1, Medical-1, Rifle-1"
  "35000 CR"]
 ["SecndOffc Gett Anne Mara (F), 46 yrs. old, merchant, 865A88"
  "Mechanical-2, Navigation-1, Brawling-1, BodyPistol-1, Electronic-2,
   Gunnery-1"
  "BodyPistol, LowPsg, LowPsg, 60000 CR (8000 CR annual)"]
 ["Lieutenant Sir Presto Rite Ashi (M), 26 yrs. old, navy, 66AA8B"
  "Engnrng-1, ShipsBoat-1, Pilot-1, Computer-1, FwdObsv-1"
  "HighPsg, LowPsg, 5000 CR"]
 ["Von Anilo Rwin Krzysztof (M), 22 yrs. old, 95696C"
  "Blade-1"
  "1000 CR"]
 ["Ross Ymour (M), 22 yrs. old, A8A777"
  "Pike-1, Brawling-1"
  "10000 CR"]
 ["Lieutenant Ndsey Tomas (M), 26 yrs. old, army, A76678"
  "Brawling-1, Gambling-1, SMG-1, Mechanical-1, ATV-1, Electronic-1, Rifle-1"
  "50000 CR"]
 ["Captain Von Essenia Tney (F), 26 yrs. old, army, 764B7C"
  "Gambling-1, SMG-1, ATV-2, Blade-1, Mechanical-2, Rifle-1"
  "HighPsg, Rifle, Rifle, 0 CR"]
 ["FourthOffc Ildred Jeremy Azuhirotoshi Spass (F), 30 yrs. old, merchant,
   499795"
  "Engnrng-1, Electronic-1, Gunnery-1, Revolver-1, Foil-1"
  "LowPsg, LowPsg, Revolver, 20000 CR"]
 ["Ms. Tena Ulie Wson (F), 22 yrs. old, marines, 7B7323"
  "Mechanical-1, Dagger-1, Cutlass-1"
  "10000 CR"]
 ["Forest (M), 22 yrs. old, navy, 69839B" "Pilot-1" "10000 CR"]
 ["Sir Rito Endan (M), 26 yrs. old, navy, 9C7586"
  "Medical-1, Gunnery-1"
  "Foil, HighPsg, 0 CR"]
 ["FourthOffc Farraine Loukas Erette Suwandi Saul (F), 62 yrs. old, merchant,
   714DA8"
  "Electronic-1, Halberd-1, Medical-1, Streetwise-1, Pike-1, AutomaticPistol-1,
   Rifle-1, Gunnery-1, Brawling-1, Computer-3"
  "Halberd, Halberd, LowPsg, LowPsg, LowPsg, 3000 CR (16000 CR annual)"]
 ["LtCmdr Hnnie Ucky Svantelisabelle (F), 42 yrs. old, navy, 571797"
  "VaccSuit-1, FwdObsv-1, Computer-1, Mechanical-1, Jack-o-T-1, Gunnery-3"
  "Cutlass, HighPsg, HighPsg, HighPsg, Travellers, 15000 CR (6000 CR annual)"]]


;; Single character, w/ all metadata:
(def our-character (make-character))

our-character

;;=>
'{:royal-form nil,
  :reinlisting? false,
  :actual-service :navy,
  :generation "I",
  :age 30,
  :commissioned? true,
  :pension-annual 0,
  :living? true,
  :rank 2,
  :first-name "Nelius",
  :surnames ["Eidi"],
  :terms-reached 3,
  :prefix "Mr.",
  :drafted? true,
  :rank-name "Lieutenant",
  :skills {Electronic 2, Gunnery 1, Engnrng 1, Mechanical 1, Dagger 1},
  :desired-service :navy,
  :credits 55000,
  :memberships #{},
  :possessions (HighPsg Dagger),
  :gender :male,
  :attributes {:ss 10, :ed 5, :in 9, :en 7, :dx 9, :st 3}}


(format-name-map our-character)

;;=>
"Lieutenant Nelius Eidi, I (M), 30 yrs. old, navy, 39795A"


(format-skills our-character)

;;=>
"Electronic-2, Gunnery-1, Engnrng-1, Mechanical-1, Dagger-1"


(format-swag our-character)

;;=>
"Dagger, HighPsg, 55000 CR"


(comment

  ;; Survival rate:
  (->> make-character
       (repeatedly 10000)
       (map :living?)
       frequencies)
  ;;=>
  {true 7141, false 2859}

  ;; Average money at mustering out time:
  (let [n 1000]
    (/ (->> make-living-character
            (repeatedly n)
            (map :credits)
            (apply +))
       n))
  ;;=>
  18750

  ;; Most common skill:
  (->> make-character
       (repeatedly 1000)
       (map :skills)
       (apply (partial merge-with +))
       (sort-by (comp - second))
       ffirst)
  ;;=>
  'Electronic

  ;; Age distributions for living characters after service:
  (->> make-living-character
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
   [94 1]])
