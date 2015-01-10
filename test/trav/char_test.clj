(ns trav.char-test
  (:require [midje.sweet :refer :all]
            [trav.char :refer :all]
            [trav.dice :refer [d]]))


(facts "about aging"
  (let [char {:age 33,
              :attributes {:ss 8, :ed 10, :in 4, :en 7, :dx 9, :st 8}}
        char34 (assoc char :age 34)
        char65 (assoc char :age 65)
        char66 (assoc char :age 66)]
    (maybe-damage-for-age char) => char
    (provided (d) => 2 :times 0)
    (maybe-damage-for-age char34) => (-> char34
                                         (assoc-in [:attributes :st] 7)
                                         (assoc-in [:attributes :dx] 8)
                                         (assoc-in [:attributes :en] 6))
    (provided (d) => 2 :times 3)
    (maybe-damage-for-age char65) => char65
    (provided (d) => 2 :times 0)
    (maybe-damage-for-age char66) => (-> char66
                                         (assoc-in [:attributes :st] 6)
                                         (assoc-in [:attributes :dx] 7)
                                         (assoc-in [:attributes :en] 5)
                                         (assoc-in [:attributes :in] 3))
    (provided (d) => 2 :times 4)))


(fact "starting-character can be run a bunch without crashing"
  (->> starting-character
       (repeatedly 400)
       (map format-name-map)
       dorun) =not=> (throws))


(fact "make-character produces something without crashing"
  (->> make-character
       (repeatedly 100)
       (map format-name-map)
       dorun) =not=> (throws))


(facts "About rank and automatic skills"
  (let [char-fn (fn [svc rank]
                  (-> (starting-character)
                      (assoc :actual-service svc)
                      (assoc :skills {})
                      (assoc :rank-name rank)))]
    (-> :army
        (char-fn "Lieutenant")
        add-automatic-skills-for-rank
        :skills) => {'SMG 1}

    (-> :merchant
        (char-fn "FirstOffc")
        add-automatic-skills-for-rank
        :skills) => {'Pilot 1}

    (-> (char-fn :navy "Captain")
        (assoc-in [:attributes :ss] 11)
        add-automatic-skills-for-rank
        :attributes
        :ss) => 12))
