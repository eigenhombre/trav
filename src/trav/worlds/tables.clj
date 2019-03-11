(ns trav.worlds.tables
  (:require [trav.dice :refer [d]]
            [trav.macros :refer [def-range-table
                                 def-zone-table]]))

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

(def-range-table starports
  r2-4 A
  r5-6 B
  r7-8 C
  9 D
  r10-11 E
  12 X)

(def-range-table spaceports
  r1-2 Y
  3 H
  r4-5 G
  6 F)

(def-zone-table size-Ia
  #_0 B0 B5 A0 A5 F0 F5 G0 G5 K0 K5 M0 M5 M9
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
  #_0 B0 B5 A0 A5 F0 F5 G0 G5 K0 K5 M0 M5 M9
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
  #_0 B0 B5 A0 A5 F0 F5 G0 G5 K0 K5 M0 M5 M9
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
  #_0 B0 B5 A0 A5 F0 F5 G0 G5 K0 K5 M0 M5 M9
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
  #_0 B0 B5 A0 A5 F0 F5 G0 G5 K0
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
  #_0 B0 B5 A0 A5 F0 F5 G0 G5 K0 K5 M0 M5 M9
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
  #_0 F5 G0 G5 K0 K5 M0 M5 M9
  0    I  I  I  I  O  O  O  O
  1    I  I  H  H  O  O  O  O
  2    I  H  O  O  O  O  O  O
  3    H  O  O  O  O  O  O  O
  4    O  O  O  O  O  O  O  O)

;; White dwarves
(def-zone-table size-D
  #_0 DB DA DF DG DK DM
  0    H  O  O  O  O  O
  1    O  O  O  O  O  O
  2    O  O  O  O  O  O
  3    O  O  O  O  O  O
  4    O  O  O  O  O  O)

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

(defn expand-dm-field [orbit-lookup-result]
  (if-let [[_ dm] (re-find #"D1\+(\d+)" (str orbit-lookup-result))]
    (+ (Integer. dm) (d 1))
    orbit-lookup-result))
