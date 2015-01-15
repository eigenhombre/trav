(ns trav.worlds
  (:require [trav.macros :refer :all]
            [trav.dice :refer [d]]))


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


;; FIXME: handle odd-sized Size IV table here...


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


(evalq
 (-> size-Ia
     (get 11)
     (get 'F5)))

;;=>
'H


(defn roll-within-range [table roll]
  (let [ks (keys table)
        max-key (apply max (keys table))
        min-key (apply min (keys table))]
    (->> max-key
         (min roll)
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


(defn make-system [& [prev-type-roll prev-size-roll]]
  (if-not prev-type-roll
    (let [type-roll (d)
          type (primary-type type-roll)
          subtype (rand-int 10)
          size-roll (d)]
      {:is-primary? true
       :type type
       :subtype subtype
       :size (get-size-for-type primary-size type subtype size-roll)
       :secondaries (repeatedly (-> (d)
                                    system-star-count
                                    dec)
                                (partial make-system type-roll size-roll))})
    (let [type-roll (+ prev-type-roll (d))
          type (roll-within-range primary-type type-roll)
          subtype (rand-int 10)
          size-roll (+ prev-size-roll (d))]
      {:is-primary? false
       :type type
       :subtype subtype
       :size (get-size-for-type secondary-size type subtype size-roll)})))


(evalq (repeatedly 10 make-system))

;;=>
'({:is-primary? true,
   :type M,
   :subtype 0,
   :size V,
   :secondaries ({:is-primary? false, :type F, :subtype 5, :size D})}
  {:is-primary? true,
   :type M,
   :subtype 7,
   :size V,
   :secondaries ({:is-primary? false, :type F, :subtype 5, :size D})}
  {:is-primary? true,
   :type K,
   :subtype 5,
   :size V,
   :secondaries ({:is-primary? false, :type F, :subtype 3, :size D})}
  {:is-primary? true,
   :type M,
   :subtype 7,
   :size V,
   :secondaries ({:is-primary? false, :type F, :subtype 3, :size D})}
  {:is-primary? true, :type M, :subtype 3, :size V, :secondaries ()}
  {:is-primary? true,
   :type M,
   :subtype 1,
   :size V,
   :secondaries ({:is-primary? false, :type M, :subtype 1, :size D})}
  {:is-primary? true, :type F, :subtype 2, :size V, :secondaries ()}
  {:is-primary? true,
   :type G,
   :subtype 1,
   :size II,
   :secondaries ({:is-primary? false, :type F, :subtype 4, :size V})}
  {:is-primary? true, :type M, :subtype 6, :size V, :secondaries ()}
  {:is-primary? true,
   :type M,
   :subtype 5,
   :size V,
   :secondaries ({:is-primary? false, :type F, :subtype 5, :size D})})
