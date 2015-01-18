(ns trav.worlds-test
  (:require [trav.worlds :refer :all]
            [midje.sweet :refer :all]))


(facts "About zone tables"
  (-> size-Ia (get 11) (get 'F5)) => 'H
  (lookup-zone 'V 'B 0 -1) => :scorched
  (lookup-zone 'V 'B 0 0) => :scorched
  (lookup-zone 'V 'B 0 11) => :inner
  (lookup-zone 'V 'B 0 12) => :habitable
  (lookup-zone 'V 'B 0 13) => :outer
  (lookup-zone 'V 'B 0 666) => :outer
  (map (partial lookup-zone 'VI 'F 7) (range 5))
  => [:inner :inner :inner :habitable :outer]
  (lookup-zone 'D 'D 'B 0) => :habitable
  (lookup-zone 'D 'D 'B 1) => :outer
  (lookup-zone 'D 'D 'K 0) => :outer)


(fact "Secondary orbits never occur INSIDE primary stars"
  (let [zone-set
        (->> make-system
             repeatedly
             ;; Only choose ones that have inside-star orbital zones
             (filter (comp (partial some #{:inside-star})
                           (partial map :zone)
                           vals
                           :orbits))
             ;; Make sure there are secondaries
             (filter (comp seq :secondaries))
             (take 20)
             ;; Compare orbits of secondaries with primary orbital zones
             (mapcat (fn [star]
                       (for [s (:secondaries star)]
                         (-> star :orbits (get (:orbit s))))))
             ;; Make sure none are inside primary star.
             (into #{}))]
    zone-set (contains :habitable)
    zone-set =not=> (contains :inside-star)))


(fact "Some orbits are empty/unavailable"
  (->> make-system
       repeatedly
       (map :orbits)
       (mapcat vals)
       (map :available)
       (take 100)
       set) => (contains false))


(future-fact "Some stars have captured planets")
