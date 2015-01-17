(ns trav.worlds-test
  (:require [trav.worlds :refer :all]
            [midje.sweet :refer :all]))


(facts "About zone tables"
  (zone-for-size-type-and-orbit 'V 'B 0 -1) => :scorched
  (zone-for-size-type-and-orbit 'V 'B 0 0) => :scorched
  (zone-for-size-type-and-orbit 'V 'B 0 11) => :inner
  (zone-for-size-type-and-orbit 'V 'B 0 12) => :habitable
  (zone-for-size-type-and-orbit 'V 'B 0 13) => :outer
  (zone-for-size-type-and-orbit 'V 'B 0 666) => :outer
  (map (partial zone-for-size-type-and-orbit 'VI 'F 7) (range 5))
  => [:inner :inner :inner :habitable :outer]
  (zone-for-size-type-and-orbit 'D 'D 'B 0) => :habitable
  (zone-for-size-type-and-orbit 'D 'D 'B 1) => :outer
  (zone-for-size-type-and-orbit 'D 'D 'K 0) => :outer)
