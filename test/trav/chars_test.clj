(ns trav.chars-test
  (:require [clojure.test :refer [deftest testing is]]
            [trav.chars :refer :all]
            [trav.dice :refer [d]]))

(deftest aging-test
  (testing "aging"
    (with-redefs [d (constantly 0)]
      (let [char {:age 33,
                  :attributes {:ss 8, :ed 10, :in 4, :en 7, :dx 9, :st 8}}
            char34 (assoc char :age 34)
            char65 (assoc char :age 65)
            char66 (assoc char :age 66)]
        (is (= (maybe-damage-for-age char) char))
        (is (= (maybe-damage-for-age char34)
               (-> char34
                   (assoc-in [:attributes :st] 7)
                   (assoc-in [:attributes :dx] 8)
                   (assoc-in [:attributes :en] 6))))

        (is (= char65 (maybe-damage-for-age char65)))
        (with-redefs [d (constantly 4)]
          (is (= (maybe-damage-for-age char66)
                 (-> char66
                     (assoc-in [:attributes :st] 6)
                     (assoc-in [:attributes :dx] 7)
                     (assoc-in [:attributes :en] 5)
                     (assoc-in [:attributes :in] 3)))))))))

(deftest starting-character-test
  (testing "starting-character can be run a bunch without crashing"
    (->> starting-character
         (repeatedly 400)
         (map format-name-map)
         dorun)))

(deftest make-character-test
  (testing "make-character produces something without crashing"
    (->> make-character
         (repeatedly 100)
         (map format-name-map)
         dorun)))

(deftest adding-skills-for-rank
  (testing "rank and automatic skills"
    (let [char-fn (fn [svc rank]
                    (-> (starting-character)
                        (assoc :actual-service svc)
                        (assoc :skills {})
                        (assoc :rank-name rank)))]
      (is (= {'SMG 1}
             (-> :army
                 (char-fn "Lieutenant")
                 add-automatic-skills-for-rank
                 :skills)))

      (is (= {'Pilot 1}
             (-> :merchant
                 (char-fn "FirstOffc")
                 add-automatic-skills-for-rank
                 :skills)))

      (is (= 12 (-> (char-fn :navy "Captain")
                    (assoc-in [:attributes :ss] 11)
                    add-automatic-skills-for-rank
                    :attributes
                    :ss))))))

(deftest mustering-out
  (testing "Being in the service earns you money"
    (is (< 1E6
           (->> make-living-character
                (repeatedly 100)
                (map :credits)
                (apply +))
           3E6)))


  (testing "Being in the service gets you stuff"
    (is (< 80
           (->> make-living-character
                (repeatedly 100)
                (mapcat :possessions)
                count)
           140)))

  (testing "Some people get Travellers' Aid Society"
    (is (->> make-living-character
             (repeatedly 100)
             (map :memberships)
             (apply concat)
             (some #{'Travellers})))))
