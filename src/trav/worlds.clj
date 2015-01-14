(ns trav.worlds
  (:require [trav.macros :refer [def-range-table
                                 evalq]]
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
