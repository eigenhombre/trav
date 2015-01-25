(ns trav.worlds-test
  (:require [trav.worlds :refer :all]
            [midje.sweet :refer :all]))


(defn- systems [] (repeatedly make-system))


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
        (->> (systems)
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


(fact "Secondaries / companions have orbits, too."
  (->> (systems)
       (take 100)
       (mapcat :secondaries)
       (map :orbits)
       count) => (roughly 50 20))


(fact "Some orbits are empty/unavailable"
  (->> (systems)
       (map :orbits)
       (mapcat vals)
       (map :available)
       (take 100)
       set) => (contains false))


(fact "Some stars have captured planets"
  (->> (systems)
       (map :orbits)
       (mapcat keys)
       (take 100)
       (remove integer?)) =not=> [])


(defn- average [s]
  (let [[n sum] (reduce (fn [[c x] [c0 x0]] [(+ c c0) (+ x x0)])
                        (map vector (repeat 1) s))]
    (/ sum n)))


(defn- num-gg [s]
  (->> s
       :planets
       (filter (comp (partial = 'GG) :type))
       count))


(facts "About GGs"
  (let [several-systems (take 100 (systems))]
    (fact "Avg. number of gas giants should be 3.3 or so"
      (->> several-systems
           (map num-gg)
           average
           double) => (roughly 3.2 0.5))

    (fact "Secondaries should have gas giants, too."
      (->> several-systems
           (map num-gg)
           average
           double) => (roughly 3.2 0.5))

    (fact "Gas giants have orbits"
      (->> several-systems
           (mapcat :planets)
           (map :orbit)
           (remove nil?)) =not=> ())))


(fact "Planetary orbits are unique"
  (->> (systems)
       (take 100)
       (map :planets)
       (map (partial map :orbit))
       ;; Only original, integer orbits:
       (map (partial filter integer?))
       (mapcat (comp vals frequencies))
       (remove #{1})) => ())


(fact "Some stars have planetoid belts"
  (->> (systems)
       (map :num-planetoids)
       (take 100)
       average
       double) => (roughly 1.41 0.3))


(facts "If a companion is present, certain restrictions on
        available orbits exist."
  (fact "In a system with companion in orbit 2, orbits 0 and 4 are available..."
    (let [orbits
          (->> (systems)
               (filter (comp (partial = 1) count :secondaries))
               (filter (comp (partial = 2) :orbit first :secondaries))
               (mapcat :orbits)
               (filter (comp true? :available second))
               (map first)
               (take 20))]
      orbits => (contains 0)
      orbits => (contains 4)
      (fact "...but orbits 1, 2, 3 are not."
        orbits =not=> (contains 1)
        orbits =not=> (contains 2)
        orbits =not=> (contains 3)))))


(fact "Orbit numbers of orbits around the companion never exceed 1/2
       the companion star's orbit number around the primary."
  (->> (systems)
       (mapcat :secondaries)
       (map (juxt :orbit (comp keys :orbits)))
       (filter (comp integer? first))
       (take 100)
       (every? (fn [[o os]] (>= o (apply max os))))) => true)
