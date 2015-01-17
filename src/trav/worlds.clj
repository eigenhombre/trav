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


(def-range-table plantetoid-present
  r1-6   yes
  r7-12  no)


(def-range-table planetoid-qty
  0      3
  r1-6   2
  r7-12  1)


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


(evalq
 (-> size-Ia
     (get 11)
     (get 'F5)))

;;=>
'H


(defn- round-for-table [n]
  (* (int (/ n 5)) 5))


(defn zone-sym-to-kw [s]
  ({'I :inner
    'O :outer
    '-- :scorched
    '- :inside-star
    'H :habitable} s))


(defn lookup-zone [size type subtype orbit]
  (if (= size 'D)  ;; Dwarf table is simple but has special column names:
    (zone-sym-to-kw
     (if (and (= type 'D)
              (= subtype 'B)
              (= orbit 0))
       'H
       'O))
    ;; Otherwise, figure out nearest appropriate row and column for lookup:
    (let [table-for-size (->> size (str "size-") symbol eval)
          available-orbits (keys table-for-size)
          [min-orbit max-orbit] (apply (juxt min max) available-orbits)
          row (max (min max-orbit orbit) min-orbit)
          subtable (table-for-size row)
          typesym #(symbol (str %1 %2))
          ;; Handle last column, e.g. M9:
          [nine-key] (filter (fn [k] (->> k str last (= \9)))
                             (keys subtable))
          k (if (= nine-key (typesym type subtype))
              nine-key
              (typesym type (round-for-table subtype)))]
      (-> k subtable zone-sym-to-kw))))


(defn roll-within-range [table roll]
  (let [ks (keys table)
        [max-key min-key] (apply (juxt max min) (keys table))]
    (->> roll
         (min max-key)
         (max min-key)
         table)))


(defn- get-size-for-type [table type subtype size-roll]
  (let [s0 (roll-within-range table size-roll)]
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
         type (roll-within-range primary-type type-roll)
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


(defn orbit-is-inside-star [{:keys [size type subtype] :as star} orbit]
  (= (lookup-zone size type subtype orbit) :inside-star))


(defn orbits [{:keys [size type subtype] :as sys}]
  (let [max-orbit (-> (d 2)
                      (+ (condp = (:size sys)
                           'III 4
                           'Ia  8
                           'Ib  8
                           'II  8
                           0))
                      (+ (condp = (:type sys)
                           'M -4
                           'K -2
                           0)))
        orbits (-> max-orbit inc range)]
    (->> orbits
         (map (partial lookup-zone size type subtype))
         (zipmap orbits)
         (assoc sys :orbits))))


(defn- expand-dm-field [orbit-lookup-result]
  (if-let [[_ dm] (re-find #"D1\+(\d+)" (str orbit-lookup-result))]
              (+ (Integer. dm) (d 1))
              orbit-lookup-result))


(defn place-companions [{:keys [secondaries
                                orbits] :as star}]
  (letfn [(set-orbit [n companion]
            (let [comp-orbit
                  (->> (d)
                       (+ (* n 4)) ;; Second companion generally further out
                       (roll-within-range companion-orbit)
                       expand-dm-field)

                  orbit (cond
                         (symbol? comp-orbit) comp-orbit
                         (orbit-is-inside-star star comp-orbit) 'Close
                         :else comp-orbit)]
              (assoc companion :orbit orbit)))]
    (assoc star :secondaries
           (map-indexed set-orbit secondaries))))


(defn make-system []
  (-> (starting-system)
      name-system
      orbits
      place-companions))


(evalq (->> make-system
            repeatedly
            (take 10)))

;;=>
'({:orbits
   {0 :habitable,
    1 :outer,
    2 :outer,
    3 :outer,
    4 :outer,
    5 :outer,
    6 :outer,
    7 :outer},
   :name "Rogue",
   :is-primary? true,
   :type M,
   :subtype 4,
   :size V,
   :secondaries ()}
  {:orbits
   {0 :inner,
    1 :inner,
    2 :habitable,
    3 :outer,
    4 :outer,
    5 :outer,
    6 :outer,
    7 :outer},
   :name "Henry",
   :is-primary? true,
   :type G,
   :subtype 6,
   :size V,
   :secondaries ()}
  {:orbits
   {0 :habitable, 1 :outer, 2 :outer, 3 :outer, 4 :outer, 5 :outer},
   :name "Amanavendra",
   :is-primary? true,
   :type M,
   :subtype 0,
   :size V,
   :secondaries ()}
  {:orbits
   {0 :habitable,
    1 :outer,
    2 :outer,
    3 :outer,
    4 :outer,
    5 :outer,
    6 :outer},
   :name "Urevis",
   :is-primary? true,
   :type M,
   :subtype 4,
   :size V,
   :secondaries
   ({:orbit 2,
     :secondaries (),
     :name "Eodore",
     :is-primary? false,
     :type F,
     :subtype 8,
     :size D})}
  {:orbits
   {0 :inner, 1 :inner, 2 :habitable, 3 :outer, 4 :outer, 5 :outer},
   :name "Roze",
   :is-primary? true,
   :type K,
   :subtype 1,
   :size V,
   :secondaries ()}
  {:orbits
   {0 :habitable,
    1 :outer,
    2 :outer,
    3 :outer,
    4 :outer,
    5 :outer,
    6 :outer},
   :name "Uane",
   :is-primary? true,
   :type K,
   :subtype 8,
   :size V,
   :secondaries ()}
  {:orbits
   {0 :inner,
    1 :inner,
    2 :inner,
    3 :inner,
    4 :inner,
    5 :inner,
    6 :habitable,
    7 :outer,
    8 :outer,
    9 :outer,
    10 :outer},
   :name "Rcarlos",
   :is-primary? true,
   :type G,
   :subtype 2,
   :size III,
   :secondaries
   ({:orbit 3,
     :secondaries (),
     :name "Alter",
     :is-primary? false,
     :type F,
     :subtype 3,
     :size V})}
  {:orbits
   {0 :inner,
    1 :inner,
    2 :habitable,
    3 :outer,
    4 :outer,
    5 :outer,
    6 :outer,
    7 :outer},
   :name "Sumu",
   :is-primary? true,
   :type K,
   :subtype 0,
   :size V,
   :secondaries ()}
  {:orbits
   {0 :habitable, 1 :outer, 2 :outer, 3 :outer, 4 :outer, 5 :outer},
   :name "Rnie",
   :is-primary? true,
   :type M,
   :subtype 3,
   :size V,
   :secondaries
   ({:orbit 7,
     :secondaries (),
     :name "Uchi",
     :is-primary? false,
     :type F,
     :subtype 2,
     :size D})}
  {:orbits {0 :inner, 1 :inner, 2 :inner, 3 :habitable, 4 :outer},
   :name "Adley",
   :is-primary? true,
   :type G,
   :subtype 4,
   :size V,
   :secondaries ()})
