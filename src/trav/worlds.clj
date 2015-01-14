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


(defn make-system [& [prev-type-roll prev-size-roll]]
  (if-not prev-type-roll
    (let [type-roll (d)
          type-subval (rand-int 10)
          size-roll (d)]
      {:is-primary? true
       :type (primary-type type-roll)
       :type-subval type-subval
       :size (primary-size size-roll)
       :secondaries (repeatedly (-> (d)
                                    system-star-count
                                    dec)
                                (partial make-system type-roll size-roll))})
    (let [type-roll (+ prev-type-roll (d))
          type-subval (rand-int 10)
          size-roll (+ prev-size-roll (d))]
      {:is-primary? false
       :type (primary-type (min type-roll (->> primary-type
                                               keys
                                               (apply max))))
       :type-subval type-subval
       :size (primary-size (min type-roll (->> primary-size
                                               keys
                                               (apply max))))})))


(evalq (repeatedly 10 make-system))

;;=>
'({:is-primary? true,
   :type G,
   :type-subval 9,
   :size V,
   :secondaries ()}
  {:is-primary? true,
   :type F,
   :type-subval 6,
   :size V,
   :secondaries ()}
  {:is-primary? true,
   :type K,
   :type-subval 4,
   :size VI,
   :secondaries ()}
  {:is-primary? true,
   :type F,
   :type-subval 5,
   :size V,
   :secondaries
   ({:is-primary? false, :type F, :type-subval 3, :size D})}
  {:is-primary? true,
   :type K,
   :type-subval 0,
   :size V,
   :secondaries
   ({:is-primary? false, :type F, :type-subval 9, :size D})}
  {:is-primary? true,
   :type M,
   :type-subval 4,
   :size V,
   :secondaries ()}
  {:is-primary? true,
   :type M,
   :type-subval 5,
   :size V,
   :secondaries
   ({:is-primary? false, :type F, :type-subval 1, :size D})}
  {:is-primary? true,
   :type G,
   :type-subval 5,
   :size V,
   :secondaries ()}
  {:is-primary? true,
   :type F,
   :type-subval 3,
   :size V,
   :secondaries ()}
  {:is-primary? true,
   :type K,
   :type-subval 2,
   :size V,
   :secondaries
   ({:is-primary? false, :type F, :type-subval 4, :size D})})

