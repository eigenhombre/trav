(ns trav.util-test
  (:require [trav.util :refer :all]
            [midje.sweet :refer :all]))


(fact "hexish-code handles values > 15 (e.g., during char/system generation)"
  (hexish-code 1) => "1"
  (hexish-code 0) => "0"
  (hexish-code -1) => (throws)
  (hexish-code 15) => "F"
  (hexish-code 16) => "G"
  (hexish-code 35) => "Z"
  (hexish-code 36) => (throws))
