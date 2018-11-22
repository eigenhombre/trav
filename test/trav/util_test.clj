(ns trav.util-test
  (:require [trav.util :refer :all]
            [clojure.test :refer [deftest testing is]]))

(deftest hexish-code-test
  (testing "hexish-code handles values > 15 (e.g., during char/system generation)"
    (is (= (hexish-code 1) "1"))
    (is (= (hexish-code 0) "0"))
    (is (thrown? AssertionError (hexish-code -1)))
    (is (= (hexish-code 15) "F"))
    (is (= (hexish-code 16) "G"))
    (is (= (hexish-code 35) "Z"))
    (is (thrown? AssertionError (hexish-code 36)))))
