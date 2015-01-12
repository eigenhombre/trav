(ns trav.worlds
  (:require [trav.macros :refer [def-range-table]]
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


(defn make-star [& [parg & _]]
  (if (= parg :primary)
    {:is-primary? true
     :type (primary-type (d))}
    {:is-primary? false
     :type (companion-type (d))}))


(defn make-system []
  (let [num-secondaries (-> (d)
                            system-star-count
                            dec)]
    {:stars {:primary (make-star :primary)
             :secondaries (repeatedly num-secondaries make-star)}}))


`(quote ~(repeatedly 10 make-system))

;;=>
'({:stars {:primary {:is-primary? true, :type M}, :secondaries ()}}
  {:stars {:primary {:is-primary? true, :type F}, :secondaries ()}}
  {:stars {:primary {:is-primary? true, :type A}, :secondaries ()}}
  {:stars {:primary {:is-primary? true, :type M}, :secondaries ()}}
  {:stars
   {:primary {:is-primary? true, :type M},
    :secondaries ({:is-primary? false, :type K})}}
  {:stars
   {:primary {:is-primary? true, :type K},
    :secondaries ({:is-primary? false, :type M})}}
  {:stars {:primary {:is-primary? true, :type M}, :secondaries ()}}
  {:stars {:primary {:is-primary? true, :type M}, :secondaries ()}}
  {:stars
   {:primary {:is-primary? true, :type M},
    :secondaries ({:is-primary? false, :type M})}}
  {:stars {:primary {:is-primary? true, :type F}, :secondaries ()}})
