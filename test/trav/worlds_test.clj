(ns trav.worlds-test
  (:require [trav.worlds :refer :all]
            [midje.sweet :refer :all]))


(fact "Systems have one or more stars"
  (-> (make-system)
      :stars
      count
      (> 1)) => true)
