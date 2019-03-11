(ns trav.worlds-test
  (:require [clojure.test :refer [deftest testing is]]
            [trav.worlds.core :as w]))

(deftest a-bunch-of-worlds
  (testing "I can make a bunch of worlds without throwing"
    (dotimes [_ 30]
      (->> (w/gen-system)
           w/system-str
           count
           pos?))))

;; FIXME: convert these to use the new namespace, or remove them:

;; (defn- systems [] (repeatedly w/make-system))

;; (deftest zone-tables
;;   (testing "zone tables"
;;     (is (= 'H (-> w/size-Ia (get 11) (get 'F5))))
;;     (is (= (w/lookup-zone 'V 'B 0 -1) :scorched))
;;     (is (= (w/lookup-zone 'V 'B 0 0) :scorched))
;;     (is (= (w/lookup-zone 'V 'B 0 11) :inner))
;;     (is (= (w/lookup-zone 'V 'B 0 12) :habitable))
;;     (is (= (w/lookup-zone 'V 'B 0 13) :outer))
;;     (is (= (w/lookup-zone 'V 'B 0 666) :outer))
;;     (is (= (w/lookup-zone 'D 'D 'B 0) :habitable))
;;     (is (= (w/lookup-zone 'D 'D 'B 1) :outer))
;;     (is (= (w/lookup-zone 'D 'D 'K 0) :outer))
;;     (is (= (map (partial w/lookup-zone 'VI 'F 7) (range 5))
;;            [:inner :inner :inner :habitable :outer]))))

;; (deftest habitable-zones
;;   (testing "secondary orbits never occur INSIDE primary stars"
;;     (let [zones
;;           (->> (systems)
;;                (filter (comp (partial some #{:inside-star})
;;                              (partial map :zone)
;;                              vals
;;                              :orbits))
;;                (filter (comp seq :secondaries))
;;                (take 100)
;;                (mapcat (fn [star]
;;                          (for [s (:secondaries star)]
;;                            (-> star :orbits (get (:orbit s))))))
;;                (map :zone)
;;                (remove nil?))]
;;       (is (not (some #{:inside-star} zones)))
;;       (is (some #{:habitable} zones)))))

;; (deftest orbits-around-secondaries
;;   (testing "Secondaries / companions have orbits, too."
;;     (< 30
;;        (->> (systems)
;;             (take 100)
;;             (mapcat :secondaries)
;;             (map :orbits)
;;             count)
;;        70)))

;; (deftest empty-orbits
;;   (testing "Some orbits are empty/unavailable"
;;     (is (->> (systems)
;;              (map :orbits)
;;              (mapcat vals)
;;              (map :available)
;;              (take 100)
;;              (remove true?)
;;              not-empty))))

;; (deftest captured-planets
;;   (testing "Some stars have captured planets"
;;     (is (->> (systems)
;;              (map :orbits)
;;              (mapcat keys)
;;              (take 100)
;;              (remove integer?)
;;              not-empty))))

;; (defn- average [s]
;;   (let [[n sum] (reduce (fn [[c x] [c0 x0]] [(+ c c0) (+ x x0)])
;;                         (map vector (repeat 1) s))]
;;     (/ sum n)))

;; (defn- num-gg [s]
;;   (->> s
;;        :planets
;;        (filter (comp (partial = :gg) :type))
;;        count))

;; (deftest gas-giants
;;   (let [several-systems (take 100 (systems))]
;;     (testing "Avg. number of gas giants should be 3.3 or so"
;;       (is (< 2.0
;;              (->> several-systems
;;                   (map num-gg)
;;                   average
;;                   double)
;;              3.7)))
;;     (testing "Secondaries should have gas giants, too."
;;       (is (< 2.0
;;              (->> several-systems
;;                   (map num-gg)
;;                   average
;;                   double)
;;              3.7)))
;;     (testing "Gas giants have orbits"
;;       (is (->> several-systems
;;                (mapcat :planets)
;;                (map :orbit)
;;                (remove nil?)
;;                not-empty)))))

;; (deftest uniqueness-of-orbits
;;   (testing "Planetary orbits are unique"
;;     (is (->> (systems)
;;              (take 100)
;;              (map :planets)
;;              (map (partial map :orbit))
;;              ;; Only original, integer orbits:
;;              (map (partial filter integer?))
;;              (mapcat (comp vals frequencies))
;;              (remove #{1})
;;              empty))))

;; (deftest restrictions-on-orbits
;;   (testing (str "In a system with companion in orbit 2, "
;;                 "orbits 0 and 4 are available...")
;;     (let [orbits
;;           (->> (systems)
;;                (filter (comp (partial = 1) count :secondaries))
;;                (filter (comp (partial = 2) :orbit first :secondaries))
;;                (mapcat :orbits)
;;                (filter (comp true? :available second))
;;                (map first)
;;                (take 20))]
;;       (is (some #{0} orbits))
;;       (is (some #{4} orbits))
;;       (testing "...but orbits 1, 2, 3 are not."
;;         (is (not (some #{1} orbits)))
;;         (is (not (some #{2} orbits)))
;;         (is (not (some #{3} orbits)))))))

;; (deftest orbit-counts
;;   (testing "numbers of orbits around the companion never exceed 1/2
;;        the companion star's orbit number around the primary."
;;     (is (->> (systems)
;;              (mapcat :secondaries)
;;              (map (juxt :orbit (comp keys :orbits)))
;;              (filter (comp seq second))  ;; Reject ones with no orbits
;;              (filter (comp integer? first))
;;              (take 100)
;;              (every? (fn [[o os]] (>= o (apply max os))))))))

;; (defn- star-has-both-gg-and-planetoid [{planets :planets}]
;;   (let [types (->> planets
;;                    (map :type)
;;                    vec)]
;;     (and (= (count types) 2)
;;          (some #{:gg} types)
;;          (some #{:planetoid} types))))


;; (defn- star-gg-has-integer-orbit [{planets :planets}]
;;   (->> planets
;;        (filter (comp (partial = :gg) :type))
;;        (filter (comp integer? :orbit))
;;        seq))


;; (deftest planetoid-belts
;;   (let [stars (take 300 (systems))]
;;     (testing "Some stars have planetoid belts"
;;       (is (some true? (->> stars
;;                            (mapcat :planets)
;;                            (map (comp (partial = :planetoid) :type))
;;                            set))))
;;     (testing "Planetoid belts are named 'Planetoid belt' and are size 0"
;;       (is (= #{["Planetoid belt" 0]}
;;              (->> stars
;;                   (mapcat :planets)
;;                   (filter (comp (partial = :planetoid) :type))
;;                   (map (juxt :name :size))
;;                   set))))
;;     (testing "When one gas giants and one planetoid belt exist, planetoid
;;            orbits are one in from GGs"
;;       (->> stars
;;            (filter star-has-both-gg-and-planetoid)
;;            (filter star-gg-has-integer-orbit)
;;            (map :planets)
;;            (map (partial sort-by :type))  ;; GGs first, then planetoid
;;            ;; Remove ones where GG has orbit of 0
;;            (remove (comp #(and (= (:type %) :gg)
;;                                (= (:orbit %) 0)) first))
;;            (map (partial map :orbit))
;;            (map (partial apply -))
;;            (take 3)
;;            prn))))  ;; You are here
