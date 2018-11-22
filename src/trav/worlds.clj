(ns trav.worlds
  (:require [namejen.names :refer [generic-name]]
            [trav.dice :refer [d]]
            [trav.macros :refer :all]))


(def-range-table system-star-count
  r0-7  1
  r8-11 2
  12    3)


(def-range-table primary-type
  r0-1   B
  2      A
  r3-7   M
  8      K
  9      G
  r10-12 F)


(def-range-table companion-type
  1      B
  2      A
  r3-4   F
  r5-6   G
  r7-8   K
  r9-12  M)


(def-range-table primary-size
  0      Ia
  1      Ib
  2      II
  3      III
  4      IV
  r5-10  V
  11     VI
  12     D)


(def-range-table secondary-size
  0      Ia
  1      Ib
  2      II
  3      III
  4      IV
  r5-6   D
  r7-8   V
  9      VI
  r10-12 D)


(def-range-table companion-orbit
  r0-3   Close
  4      1
  5      2
  6      3
  7      D1+4
  8      D1+5
  9      D1+6
  10     D1+7
  11     D1+8
  12     Far)


(def-range-table gas-giant-present
  r1-9   yes
  r10-12 no)


(def-range-table gas-giant-qty
  r1-3   1
  r4-5   2
  r6-7   3
  r8-10  4
  r9-12  5)


(def-range-table planetoid-present
  r1-6   yes
  r7-12  no)


(def-range-table planetoid-qty
  0      3
  r1-6   2
  r7-12  1)


(def-range-table have-empty-orbits
  r1-4 no
  r5-6 yes)


(def-range-table num-empty-orbits
  r1-2 1
  3    2
  r4-6 3)


(def-range-table have-captured-planets
  r1-4 no
  r5-6 yes)


(def-range-table num-captured-planets
  r1-2 1
  r3-4 2
  r5-6 3)


(def-zone-table size-Ia
      B0 B5 A0 A5 F0 F5 G0 G5 K0 K5 M0 M5 M9
  1   -- --  -  -  -  -  -  -  -  -  -  -  -
  2   -- -- -- --  -  -  -  -  -  -  -  -  -
  3   -- -- -- -- -- --  -  -  -  -  -  -  -
  4   -- -- -- -- -- -- -- --  -  -  -  -  -
  5   -- -- -- -- -- -- -- --  -  -  -  -  -
  6   -- -- -- --  I  I -- -- -- --  -  -  -
  7   --  I  I  I  I  I  I  I  I  I  I  -  -
  8    I  I  I  I  I  I  I  I  I  I  I  I  I
  9    I  I  I  I  I  I  I  I  I  I  I  I  I
  10   I  I  I  I  I  I  I  I  I  I  I  I  I
  11   I  I  I  I  I  H  I  I  I  I  I  I  I
  12   I  H  H  H  H  H  H  H  H  H  H  H  H
  13   H  O  O  O  O  O  O  O  O  O  O  O  O
  14   O  O  O  O  O  O  O  O  O  O  O  O  O)


(def-zone-table size-Ib
      B0 B5 A0 A5 F0 F5 G0 G5 K0 K5 M0 M5 M9
  1   -- -- -- -- -- -- --  -  -  -  -  -  -
  2   -- -- -- -- -- -- -- --  -  -  -  -  -
  3   -- -- -- -- -- -- -- --  -  -  -  -  -
  4   -- -- -- -- --  I  I --  -  -  -  -  -
  5   -- --  I  I  I  I  I  I  I --  -  -  -
  6   --  I  I  I  I  I  I  I  I  I  I  -  -
  7   --  I  I  I  I  I  I  I  I  I  I  I  -
  8    I  I  I  I  I  I  I  I  I  I  I  I  I
  9    I  I  I  I  I  I  I  I  I  I  I  I  I
  10   I  I  I  H  H  H  H  H  H  I  I  I  I
  11   I  H  H  O  O  O  O  O  O  H  H  I  I
  12   I  O  O  O  O  O  O  O  O  O  O  H  H
  13   H  O  O  O  O  O  O  O  O  O  O  O  O
  14   O  O  O  O  O  O  O  O  O  O  O  O  O)


(def-zone-table size-II
      B0 B5 A0 A5 F0 F5 G0 G5 K0 K5 M0 M5 M9
  1   -- -- -- -- -- -- -- -- --  -  -  -  -
  2   -- -- --  I  I  I  I  I  I --  -  -  -
  3   -- --  I  I  I  I  I  I  I  I  -  -  -
  4   -- --  I  I  I  I  I  I  I  I  I  -  -
  5   --  I  I  I  I  I  I  I  I  I  I  -  -
  6   --  I  I  I  I  I  I  I  I  I  I  I  I
  7    I  I  I  I  I  I  I  I  I  I  I  I  I
  8    I  I  I  H  H  H  H  H  I  I  I  I  I
  9    I  I  H  O  O  O  O  O  H  H  I  I  I
  10   I  I  O  O  O  O  O  O  O  O  H  I  I
  11   I  H  O  O  O  O  O  O  O  O  O  H  H
  12   H  O  O  O  O  O  O  O  O  O  O  O  O
  13   O  O  O  O  O  O  O  O  O  O  O  O  O)


(def-zone-table size-III
      B0 B5 A0 A5 F0 F5 G0 G5 K0 K5 M0 M5 M9
  1   -- --  I  I  I  I  I  I  I  I --  -  -
  2   -- --  I  I  I  I  I  I  I  I  I  -  -
  3   -- --  I  I  I  I  I  I  I  I  I  -  -
  4   -- --  I  I  I  I  I  I  I  I  I  I  -
  5   --  I  I  I  I  I  I  I  I  I  I  I  I
  6   --  I  I  I  H  H  H  I  I  I  I  I  I
  7    I  I  I  H  O  O  O  H  H  I  I  I  I
  8    I  I  O  O  O  O  O  O  O  H  H  I  I
  9    I  I  H  O  O  O  O  O  O  O  O  H  H
  10   I  H  O  O  O  O  O  O  O  O  O  O  O
  11   I  O  O  O  O  O  O  O  O  O  O  O  O
  12   H  O  O  O  O  O  O  O  O  O  O  O  O
  13   O  O  O  O  O  O  O  O  O  O  O  O  O)


(def-zone-table size-IV
      B0 B5 A0 A5 F0 F5 G0 G5 K0
  0   -- -- --  I  I  I  I  I  I
  1   -- --  I  I  I  I  I  I  I
  2   -- --  I  I  I  I  I  I  I
  3   --  I  I  I  I  I  I  I  I
  4   --  I  I  I  I  I  I  I  H
  5   --  I  I  I  I  H  H  H  O
  6   --  I  I  H  H  O  O  O  O
  7    I  I  H  O  O  O  O  O  O
  8    I  I  O  O  O  O  O  O  O
  9    I  H  O  O  O  O  O  O  O
  10   I  O  O  O  O  O  O  O  O
  11   I  O  O  O  O  O  O  O  O
  12   H  O  O  O  O  O  O  O  O
  13   O  O  O  O  O  O  O  O  O)


(def-zone-table size-V
      B0 B5 A0 A5 F0 F5 G0 G5 K0 K5 M0 M5 M9
  0   -- --  I  I  I  I  I  I  I  H  H  O  O
  1   -- --  I  I  I  I  I  I  I  O  O  O  O
  2   -- --  I  I  I  I  I  H  H  O  O  O  O
  3   -- --  I  I  I  I  H  O  O  O  O  O  O
  4   --  I  I  I  I  H  O  O  O  O  O  O  O
  5   --  I  I  I  H  O  O  O  O  O  O  O  O
  6    I  I  I  H  O  O  O  O  O  O  O  O  O
  7    I  I  H  O  O  O  O  O  O  O  O  O  O
  8    I  I  O  O  O  O  O  O  O  O  O  O  O
  9    I  H  O  O  O  O  O  O  O  O  O  O  O
  10   I  O  O  O  O  O  O  O  O  O  O  O  O
  11   I  O  O  O  O  O  O  O  O  O  O  O  O
  12   H  O  O  O  O  O  O  O  O  O  O  O  O
  13   O  O  O  O  O  O  O  O  O  O  O  O  O)


(def-zone-table size-VI ;; sub-dwarf
      F5 G0 G5 K0 K5 M0 M5 M9
  0    I  I  I  I  O  O  O  O
  1    I  I  H  H  O  O  O  O
  2    I  H  O  O  O  O  O  O
  3    H  O  O  O  O  O  O  O
  4    O  O  O  O  O  O  O  O)


;; White dwarves
(def-zone-table size-D
     DB DA DF DG DK DM
 0    H  O  O  O  O  O
 1    O  O  O  O  O  O
 2    O  O  O  O  O  O
 3    O  O  O  O  O  O
 4    O  O  O  O  O  O)


(defn- round-subtype-to-nearest-mult-of-five [subtype]
  (* (int (/ subtype 5)) 5))


(defn bracketed-lookup
  "
  Look up <roll> in <table>, handling over/underflows gracefully.
  "
  [table roll]
  (let [ks (keys table)
        [max-key min-key] (apply (juxt max min) (keys table))]
    (->> roll
         (min max-key)
         (max min-key)
         table)))


(defn zone-sym-to-kw [s]
  ({'I :inner
    'O :outer
    '-- :scorched
    '- :inside-star
    'H :habitable} s))


(defn zone-table-for-size [size]
  (->> size
       (str "trav.worlds/size-")
       symbol
       find-var
       var-get))

(defn zone-for-dwarf
  "
  Dwarf table is simple but has special column names.  Just hard-code
  the table for now.
  "
  [type subtype orbit]
  (zone-sym-to-kw (if (and (= type 'D)
                           (= subtype 'B)
                           (= orbit 0))
                    'H
                    'O)))


(defn lookup-zone [size type subtype orbit]
  (if (= size 'D)
    (zone-for-dwarf type subtype orbit)
    ;; Otherwise, figure out nearest appropriate row and column for lookup:
    (let [zone-table (zone-table-for-size size)
          available-orbits (keys zone-table)
          [min-orbit max-orbit] (apply (juxt min max) available-orbits)
          row (->> orbit
                   (min max-orbit)
                   (max min-orbit))
          subtable (zone-table row)
          typesym #(symbol (str %1 %2))
          ;; Handle last column, e.g. M9:
          [nine-key] (filter (fn [k] (->> k str last (= \9)))
                             (keys subtable))
          k (if (= nine-key (typesym type subtype))
              nine-key
              (typesym type (round-subtype-to-nearest-mult-of-five subtype)))]
      (-> k subtable zone-sym-to-kw))))


(defn- get-size-for-type [table type subtype size-roll]
  (let [s0 (bracketed-lookup table size-roll)]
    (cond
     (and (= s0 'IV)
          (or (and (= type 'K)
                   (> subtype 4))
              (= type 'M)))
     'V

     (and (= s0 'VI)
          (or (= type 'A)
              (= type 'B)
              (and (= type 'F)
                   (< subtype 5))))
     'V
     :else s0)))


(defn starting-system
  ([]
   (let [type-roll (d)
         type (primary-type type-roll)
         subtype (rand-int 10)
         size-roll (d)
         secondaries (repeatedly (-> (d)
                                     system-star-count
                                     dec)
                                 (partial starting-system
                                          type-roll
                                          size-roll))]
     {:is-primary? true
      :type type
      :subtype subtype
      :size (get-size-for-type primary-size type subtype size-roll)
      :secondaries secondaries}))
  ([prev-type-roll prev-size-roll]
   (let [type-roll (+ prev-type-roll (d))
         type (bracketed-lookup primary-type type-roll)
         subtype (rand-int 10)
         size-roll (+ prev-size-roll (d))]
     {:is-primary? false
      :type type
      :subtype subtype
      :size (get-size-for-type secondary-size type subtype size-roll)})))


(defn name-system [sys]
  (-> sys
      (assoc :name (generic-name))
      (assoc :secondaries (map name-system (:secondaries sys)))))


(defn orbits [{:keys [is-primary? size type subtype] :as star}]
  (let [max-orbit (-> (d 2)
                      (+ (condp = (:size star)
                           'III 4
                           'Ia  8
                           'Ib  8
                           'II  8
                           0))
                      (+ (condp = (:type star)
                           'M -4
                           'K -2
                           0)))
        orbit-around-primary (:orbit star)
        max-orbit (if (and (not is-primary?)
                           (integer? orbit-around-primary))
                    (min max-orbit (int (/ orbit-around-primary 2)))
                    max-orbit)
        orbits-seq (range 0 (inc max-orbit))
        orbit-map (zipmap orbits-seq
                          (map (fn [o] {:zone (lookup-zone size type subtype o)
                                        :available true})
                                 orbits-seq))]
    (-> star
        (assoc :orbits orbit-map)
        (assoc :secondaries (map orbits (:secondaries star))))))


(defn- expand-dm-field [orbit-lookup-result]
  (if-let [[_ dm] (re-find #"D1\+(\d+)" (str orbit-lookup-result))]
              (+ (Integer. dm) (d 1))
              orbit-lookup-result))


(defn orbit-is-inside-star [{:keys [size type subtype] :as star} orbit]
  (= (lookup-zone size type subtype orbit) :inside-star))


(defn place-companions [{:keys [secondaries
                                orbits] :as star}]
  (letfn [(set-orbit [n companion]
            (let [comp-orbit
                  (->> (d)
                       (+ (* n 4)) ;; Second companion generally further out
                       (bracketed-lookup companion-orbit)
                       expand-dm-field)

                  orbit (cond
                         (symbol? comp-orbit) comp-orbit
                         (orbit-is-inside-star star comp-orbit) 'Close
                         :else comp-orbit)]
              (assoc companion :orbit orbit)))]
    (assoc star :secondaries
           (map-indexed set-orbit secondaries))))


(defn available-orbit-numbers [star]
  (->> star
       :orbits
       (filter (comp (partial = true) :available second))
       keys))


(defn capture-and-empty-table-dm [star]
  (if (#{'B 'A} (:type star))
    1
    0))


(defn prune-orbits
  "
  Remove some orbits, sometimes, based on *-empty-orbits tables,
  above.  We deviate from the rules slightly by choosing at random
  which orbit to delete, rather than rolling (and probably re-rolling)
  2D.  This shifts the probability slightly but I see no reason to
  follow a weighted-in-the-middle distribution for this.
  "
  [star]
  (let [dm (capture-and-empty-table-dm star)]
    (if (= (bracketed-lookup have-empty-orbits
                             (+ (d 1) dm))
           'yes)
      (loop [star star
             ne (bracketed-lookup num-empty-orbits
                                  (+ (d 1) dm))]
        (let [orbs (available-orbit-numbers star)]
          (cond (zero? ne) star
                (empty? orbs) star
                :else (recur (assoc-in star [:orbits
                                             (rand-nth orbs)
                                             :available]
                                       false)
                             (dec ne)))))
      star)))


(defn forbidden-range-for-orbit [n]
  (if-not (integer? n)
    []
    [(- n (int (/ n 2)))
     (+ n (int (/ n 2)))]))


(defn remove-orbits-screened-by-companions
  "
  Remove orbits from primaries 'screened' (my term) from existence by
  presence of companion in nearby orbital slot.  For all companions,
  use companion orbit position to determine forbidden nearby slots.
  "
  [star]
  (let [companion-orbits (->> star
                              :secondaries
                              (map :orbit))
        forbidden-ranges (map forbidden-range-for-orbit companion-orbits)
        disallowed-fn (fn f [n [[lo hi] & more]]
                        (if (nil? lo)
                          false
                          (or (<= lo n hi)
                              (f n more))))]
    (-> star
        (assoc :orbits
          (into {}
                (for [[o m] (:orbits star)]
                  (if (disallowed-fn o forbidden-ranges)
                    [o (assoc m :available false)]
                    [o m]))))
        (assoc :secondaries (map remove-orbits-screened-by-companions
                                 (:secondaries star))))))


(defn captured-orbit-details [{:keys [size type subtype]}]
  (let [whole-orbit (d)
        zone (lookup-zone size type subtype whole-orbit)]
    {(-> whole-orbit
         (+ (* 0.1 (- (d) 7))))
     {:available true
      :zone zone}}))


(defn add-capture-orbits [star]
  (let [dm (capture-and-empty-table-dm star)]
    (if (= (bracketed-lookup have-captured-planets
                             (+ (d 1) dm)))
      (let [nc (bracketed-lookup num-captured-planets
                                 (+ (d 1) dm))]
        (update-in star [:orbits] merge
                   (->> (partial captured-orbit-details star)
                        (repeatedly nc)
                        (reduce merge))))
      star)))


(defn determine-gas-giant-qty [star]
  (let [num-gg (if-not (= (gas-giant-present (d)))
                 0
                 (gas-giant-qty (d)))]
    (-> star
        (assoc :num-gg num-gg)
        (assoc :secondaries
          (map determine-gas-giant-qty (:secondaries star))))))


(defn determine-planetoid-qty [star]
  (let [num-planetoids (if-not (= (planetoid-present (d)))
                         0
                         (planetoid-qty (d)))]
    (-> star
        (assoc :num-planetoids num-planetoids)
        (assoc :secondaries
          (map determine-planetoid-qty (:secondaries star))))))


(defn available-orbits [star]
  (->> star
       :orbits
       (filter (comp :available second))))


(defn gg-preferred-orbits
  "
  'While gas giants can be in inner orbits, they should not be placed
  starward of the habitable zone unless there are no other orbits
  available.'
  "
  [star]
  (->> star
       :orbits
       (filter (comp :available second))
       (filter (comp #{:outer :habitable} :zone second))))


(defn place-ggs
  "
  Deviation from the rules: if no orbit is available, we discard the GG.
  "
  [{:keys [num-gg orbits] :as star}]
  (if-not (pos? num-gg)
    star
    (let [preferred (gg-preferred-orbits star)
          choices (if (seq preferred)
                    preferred
                    (available-orbits star))]
      (-> (if-not (seq choices)
            star
            (let [choicenums (keys choices)
                  o (rand-nth choicenums)]
              (-> star
                  (update-in [:planets] conj {:type :gg
                                              :name (generic-name)
                                              :size (rand-nth [:small :large])
                                              :orbit o})
                  (assoc-in [:orbits o :available] false))))
          (update-in [:num-gg] dec)
          (assoc :secondaries
            (map place-ggs (:secondaries star)))
          place-ggs))))


(defn place-planetoid-belts [{:keys [num-planetoids orbits] :as star}]
  (if-not (pos? num-planetoids)
    star
    (let [orbit-choices (available-orbits star)]
      (-> (if-not (seq orbit-choices)
            star
            (let [o (rand-nth (keys orbit-choices))]
              (-> star
                  (update-in [:planets] conj {:type :planetoid
                                              :name "Planetoid belt"
                                              :orbit o
                                              :size 0})
                  (assoc-in [:orbits o :available] false))))
          (update-in [:num-planetoids] dec)
          (assoc :secondaries
            (map place-planetoid-belts (:secondaries star)))
          place-planetoid-belts))))


(defn make-system []
  (-> (starting-system)
      name-system
      place-companions
      orbits
      prune-orbits
      remove-orbits-screened-by-companions
      add-capture-orbits
      determine-gas-giant-qty
      determine-planetoid-qty
      place-ggs
      place-planetoid-belts))


(defn format-planet [planet]
  (condp = (:type planet)
    'GG (format " %-9s %-10s %s"
                (:orbit planet)
                (:name planet)
                (str (if (= (:size planet) :small) "Small" "Large")
                     " GG"))
    "...something else..."))


(defn format-star [{:keys [is-primary?
                           name
                           size
                           type
                           subtype]}]
  (format "%-10s %-10s %s"
          (if is-primary? "Primary" "Companion")
          name
          (str type subtype " " size)))


(defn format-system [star]
  (clojure.string/join
   "\n"
   (list* (format "%-10s %-10s %-15s %-1s"
                  "Orbit"
                  "Name"
                  "UPP"
                  "Remarks")
          (format-star star)
          (concat (map format-planet (sort-by :orbit (:planets star)))
                  (mapcat
                   #(list* (format-star %)
                           (->> %
                                :planets
                                (sort-by :orbit)
                                (map format-planet)))
                   (:secondaries star))))))


(evalq (-> (make-system)
           format-system
           (clojure.string/split #"\n")))

;;=>
'["Orbit      Name       UPP             Remarks"
  "Primary    Shamilton  M5 II"
  " 0         Arsha      Small GG"
  " 1         Ldip       Small GG"
  " 4         Charlie    Small GG"
  " 6.4       Rold       Large GG"
  " 7.4       Nford      Small GG"
  "Companion  Tanya      K1 VI"
  " 1         Thomas     Small GG"
  " 2         Wahar      Large GG"
  " 3         Slartin    Small GG"
  "Companion  Aananda    K6 VI"
  " 0         Madoss     Large GG"
  " 2         Slav       Small GG"]



(evalq (->> make-system
            repeatedly
            (take 15)
            (mapcat (juxt (comp vector format-star)
                          (comp (partial map format-star) :secondaries)))
            concat
            (remove empty?)))

;;=>
'(["Primary    Nifer      M0 V"]
  ["Primary    Alejandrea M9 V"]
  ["Primary    Akash      M6 V"]
  ["Primary    Ragnar     F2 V"]
  ["Primary    Kimmo      M3 V"]
  ["Primary    Hall       M9 III"]
  ["Primary    Leads      M7 V"]
  ["Primary    Nrichael   K1 V"]
  ("Companion  Inley      F4 D")
  ["Primary    Olis       K4 V"]
  ["Primary    Rgaret     G9 IV"]
  ["Primary    Slartin    M2 V"]
  ["Primary    Christie   M3 V"]
  ["Primary    Driannette K6 V"]
  ("Companion  Arvey      F6 D")
  ["Primary    Ucifer     G8 V"]
  ["Primary    Incenzo    M6 VI"]
  ("Companion  Neal       F7 D"))



(evalq (->> make-system
            repeatedly
            (take 10)))

;;=>
'({:secondaries (),
   :planets
   ({:type GG, :name "Sridhar", :size :large, :orbit 4}
    {:type GG, :name "Nder", :size :small, :orbit 6}),
   :num-gg 0,
   :name "Frey",
   :type M,
   :size VI,
   :orbits
   {7.8 {:available true, :zone :outer},
    5.7 {:available true, :zone :outer},
    6 {:zone :outer, :available false},
    5 {:zone :outer, :available true},
    4 {:zone :outer, :available false},
    3 {:zone :outer, :available true},
    2 {:zone :outer, :available true},
    1 {:zone :outer, :available true},
    0 {:zone :outer, :available true}},
   :num-planetoids 2,
   :is-primary? true,
   :subtype 7}
  {:secondaries
   ({:secondaries (),
     :planets
     ({:type GG, :name "Tefan", :size :small, :orbit 3}
      {:type GG, :name "Inhard", :size :large, :orbit 1}
      {:type GG, :name "Entonella", :size :small, :orbit 0}
      {:type GG, :name "Gunter", :size :large, :orbit 2}),
     :num-gg 0,
     :orbit 6,
     :name "Iannette",
     :type F,
     :size D,
     :orbits
     {3 {:zone :outer, :available false},
      2 {:zone :outer, :available false},
      1 {:zone :outer, :available false},
      0 {:zone :outer, :available false}},
     :num-planetoids 1,
     :is-primary? false,
     :subtype 6}),
   :planets
   ({:type GG, :name "Buck", :size :small, :orbit 1}
    {:type GG, :name "Dennifer", :size :large, :orbit 9.5}
    {:type GG, :name "Ocorrito", :size :small, :orbit 0}
    {:type GG, :name "Eliza", :size :small, :orbit 5.4}),
   :num-gg 0,
   :name "Rtis",
   :type M,
   :size V,
   :orbits
   {9.5 {:available false, :zone :outer},
    5.4 {:available false, :zone :outer},
    1 {:zone :outer, :available false},
    0 {:zone :habitable, :available false}},
   :num-planetoids 2,
   :is-primary? true,
   :subtype 4}
  {:secondaries
   ({:secondaries (),
     :planets ({:type GG, :name "Stic", :size :small, :orbit 0}),
     :num-gg 0,
     :orbit 1,
     :name "Presley",
     :type F,
     :size D,
     :orbits {0 {:zone :outer, :available false}},
     :num-planetoids 1,
     :is-primary? false,
     :subtype 8}),
   :planets
   ({:type GG, :name "Icah", :size :large, :orbit 2}
    {:type GG, :name "Igurd", :size :small, :orbit 9.1}
    {:type GG, :name "Ienz", :size :large, :orbit 4}),
   :num-gg 0,
   :name "Rotoshi",
   :type K,
   :size VI,
   :orbits
   {9.1 {:available false, :zone :outer},
    5 {:zone :outer, :available false},
    4 {:zone :outer, :available false},
    3 {:zone :outer, :available false},
    2 {:zone :outer, :available false},
    1 {:zone :habitable, :available false},
    0 {:zone :inner, :available false}},
   :num-planetoids 1,
   :is-primary? true,
   :subtype 2}
  {:secondaries (),
   :planets ({:type GG, :name "Vised", :size :small, :orbit 7.0}),
   :num-gg 0,
   :name "Pascal",
   :type M,
   :size V,
   :orbits
   {5.2 {:available true, :zone :outer},
    7.0 {:available false, :zone :outer},
    4 {:zone :outer, :available true},
    3 {:zone :outer, :available true},
    2 {:zone :outer, :available true},
    1 {:zone :outer, :available true},
    0 {:zone :outer, :available true}},
   :num-planetoids 2,
   :is-primary? true,
   :subtype 7}
  {:secondaries (),
   :planets
   ({:type GG, :name "Paula", :size :small, :orbit 2}
    {:type GG, :name "Ofoklis", :size :small, :orbit 0}
    {:type GG, :name "Ilner", :size :small, :orbit 10.0}),
   :num-gg 0,
   :name "Duane",
   :type M,
   :size V,
   :orbits
   {10.0 {:available false, :zone :outer},
    8.8 {:available true, :zone :outer},
    6.5 {:available true, :zone :outer},
    2 {:zone :outer, :available false},
    1 {:zone :outer, :available true},
    0 {:zone :outer, :available false}},
   :num-planetoids 2,
   :is-primary? true,
   :subtype 6}
  {:secondaries (),
   :planets
   ({:type GG, :name "Jussi", :size :large, :orbit 5}
    {:type GG, :name "Aire", :size :small, :orbit 11.0}),
   :num-gg 0,
   :name "Nacea",
   :type F,
   :size V,
   :orbits
   {11.0 {:available false, :zone :outer},
    5.5 {:available true, :zone :habitable},
    5 {:zone :habitable, :available false},
    4 {:zone :inner, :available true},
    3 {:zone :inner, :available true},
    2 {:zone :inner, :available true},
    1 {:zone :inner, :available true},
    0 {:zone :inner, :available true}},
   :num-planetoids 1,
   :is-primary? true,
   :subtype 3}
  {:secondaries
   ({:secondaries (),
     :planets ({:type GG, :name "Stopher", :size :small, :orbit 0}),
     :num-gg 0,
     :orbit 1,
     :name "Mara",
     :type F,
     :size D,
     :orbits {0 {:zone :outer, :available false}},
     :num-planetoids 2,
     :is-primary? false,
     :subtype 2}),
   :planets
   ({:type GG, :name "Utoku", :size :small, :orbit 8}
    {:type GG, :name "Nigel", :size :small, :orbit 9.5}
    {:type GG, :name "Mary", :size :large, :orbit 6}
    {:type GG, :name "Ahul", :size :large, :orbit 4}
    {:type GG, :name "Rajiv", :size :small, :orbit 7}),
   :num-gg 0,
   :name "Enson",
   :type F,
   :size V,
   :orbits
   {0 {:zone :inner, :available false},
    9.5 {:available false, :zone :outer},
    7.6 {:available true, :zone :outer},
    3.7 {:available true, :zone :habitable},
    7 {:zone :outer, :available false},
    1 {:zone :inner, :available false},
    4 {:zone :habitable, :available false},
    6 {:zone :outer, :available false},
    3 {:zone :inner, :available true},
    2 {:zone :inner, :available true},
    9 {:zone :outer, :available false},
    5 {:zone :outer, :available true},
    10 {:zone :outer, :available false},
    8 {:zone :outer, :available false}},
   :num-planetoids 2,
   :is-primary? true,
   :subtype 5}
  {:secondaries (),
   :planets
   ({:type GG, :name "Ericky", :size :small, :orbit 8}
    {:type GG, :name "Arah", :size :large, :orbit 2}
    {:type GG, :name "Susan", :size :small, :orbit 0}),
   :num-gg 0,
   :name "Keuchi",
   :type M,
   :size V,
   :orbits
   {0 {:zone :outer, :available false},
    6.0 {:available true, :zone :outer},
    7 {:zone :outer, :available true},
    1 {:zone :outer, :available true},
    4 {:zone :outer, :available true},
    6 {:zone :outer, :available true},
    3 {:zone :outer, :available true},
    2 {:zone :outer, :available false},
    5 {:zone :outer, :available true},
    8 {:zone :outer, :available false}},
   :num-planetoids 1,
   :is-primary? true,
   :subtype 5}
  {:secondaries (),
   :planets ({:type GG, :name "Murat", :size :small, :orbit 0}),
   :num-gg 0,
   :name "Hawn",
   :type M,
   :size V,
   :orbits
   {7.8 {:available true, :zone :outer},
    7.4 {:available true, :zone :outer},
    2 {:zone :outer, :available true},
    1 {:zone :outer, :available false},
    0 {:zone :outer, :available false}},
   :num-planetoids 1,
   :is-primary? true,
   :subtype 6}
  {:secondaries (),
   :planets
   ({:type GG, :name "Earnix", :size :large, :orbit 7}
    {:type GG, :name "Ason", :size :large, :orbit 9}),
   :num-gg 0,
   :name "Ritz",
   :type F,
   :size VI,
   :orbits
   {0 {:zone :inner, :available true},
    5.8 {:available true, :zone :outer},
    7 {:zone :outer, :available false},
    1 {:zone :inner, :available true},
    4 {:zone :outer, :available false},
    6 {:zone :outer, :available true},
    3 {:zone :habitable, :available true},
    2 {:zone :inner, :available false},
    9 {:zone :outer, :available false},
    5 {:zone :outer, :available true},
    8 {:zone :outer, :available true}},
   :num-planetoids 2,
   :is-primary? true,
   :subtype 7})
