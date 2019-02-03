(ns trav.worlds.core
  (:require [namejen.names :refer [generic-name]]
            [trav.dice :refer [d]]
            [trav.worlds.tables :as t]))

(defn- bracketed-lookup
  "
  Look up <roll> in <table>, handling over/underflows gracefully.
  "
  [table roll]
  (let [ks (keys table)
        [max-key min-key] (apply (juxt max min) (keys table))]
    (->> roll
         (min max-key)
         (max min-key)
         table)))

(defn- get-size-for-type [table type_ subtype size-roll]
  (let [s0 (bracketed-lookup table size-roll)]
    (cond
      (and (= s0 'IV)
           (or (and (= type_ 'K)
                    (> subtype 4))
               (= type_ 'M)))
      'V
      (and (= s0 'VI)
           (or (= type_ 'A)
               (= type_ 'B)
               (and (= type_ 'F)
                    (< subtype 5))))
      'V
      :else s0)))

(defn stars
  ([]
   (let [type-roll (d)
         type_ (t/primary-type type-roll)
         subtype (rand-int 10)
         size-roll (d)]
     (cons {:is-primary? true
            :type_ type_
            :subtype subtype
            :size (get-size-for-type t/primary-size type_ subtype size-roll)}
           (repeatedly (-> (d) t/system-star-count dec)
                       (partial stars type-roll size-roll)))))
  ([prev-type-roll prev-size-roll]
   (let [type-roll (+ prev-type-roll (d))
         type_ (bracketed-lookup t/primary-type type-roll)
         subtype (rand-int 10)
         size-roll (+ prev-size-roll (d))]
     {:is-primary? false
      :type_ type_
      :subtype subtype
      :size (get-size-for-type t/secondary-size type_ subtype size-roll)})))

(defn name-system [sys]
  (assoc sys :name (generic-name)))

(defn zone-sym-to-kw [s]
  ({'I :inner
    'O :outer
    '-- :scorched
    '- :inside-star
    'H :habitable} s))

(defn zone-for-dwarf
  "
  Dwarf table is simple but has special column names.  Just hard-code
  the table for now.
  "
  [type_ subtype orbit]
  (zone-sym-to-kw (if (and (= type_ 'D)
                           (= subtype 'B)
                           (= orbit 0))
                    'H
                    'O)))

(defn zone-table-for-size [size]
  (->> size
       (str "trav.worlds.tables/size-")
       symbol
       find-var
       var-get))

(defn- round-subtype-to-nearest-mult-of-five [subtype]
  (* (int (/ subtype 5)) 5))

(defn lookup-zone [size type_ subtype orbit]
  (if (= size 'D)
    (zone-for-dwarf type_ subtype orbit)
    ;; Otherwise, figure out nearest appropriate row and column for lookup:
    (let [zone-table (zone-table-for-size size)
          available-orbits (keys zone-table)
          [min-orbit max-orbit] (apply (juxt min max) available-orbits)
          row (->> orbit
                   (min max-orbit)
                   (max min-orbit))
          subtable (zone-table row)
          typesym #(symbol (str %1 %2))
          ;; Handle last column, e.g. M9:
          [nine-key] (filter (fn [k] (->> k str last (= \9)))
                             (keys subtable))
          k (if (= nine-key (typesym type_ subtype))
              nine-key
              (typesym type_ (round-subtype-to-nearest-mult-of-five subtype)))]
      (-> k subtable zone-sym-to-kw))))

(defn orbit-is-inside-star [{:keys [size type_ subtype] :as star} orbit]
  (= :inside-star (lookup-zone size type_ subtype orbit)))

(defn set-companion-orbits [stars]
  (let [companions (remove :is-primary? stars)
        primary (first (filter :is-primary? stars))
        set-orbit (fn [n companion]
                    (let [comp-orbit
                          (->> (d)
                               ;; Second companion generally further out:
                               (+ (* n 4))
                               (bracketed-lookup t/companion-orbit)
                               t/expand-dm-field)
                          orbit (cond
                                  (symbol? comp-orbit) comp-orbit

                                  (orbit-is-inside-star primary comp-orbit)
                                  'Close

                                  :else comp-orbit)]
                      (assoc companion :orbit orbit)))]
    (cons primary (map-indexed set-orbit companions))))

(defn max-star-orbit [star]
  (-> (d)
      (+ (condp = (:size star)
           'III 4
           'Ia  8
           'Ib  8
           'II  8
           0))
      (+ (condp = (:type_ star)
           'M -4
           'K -2
           0))))

(defn orbits [{:keys [is-primary? size type_ subtype orbit] :as star}]
  (let [max-orbit (-> (d)
                      (+ (condp = size
                           'III 4
                           'Ia  8
                           'Ib  8
                           'II  8
                           0))
                      (+ (condp = type_
                           'M -4
                           'K -2
                           0)))
        orbit-around-primary (:orbit star)
        max-orbit (if (and (not is-primary?)
                           orbit-around-primary)
                    (min max-orbit (int (/ orbit-around-primary 2)))
                    max-orbit)
        orbits-seq (range 0 (inc max-orbit))
        orbit-map (zipmap orbits-seq
                          (map (fn [o] {:zone (lookup-zone size type_ subtype o)
                                        :available true})
                               orbits-seq))]
    (assoc star :orbits )
    (-> star
        (assoc :orbits orbit-map)
        (assoc :secondaries (map orbits (:secondaries star))))))

(defn orbit-allowed-for-primary [primary-orbit orbit]
  {:pre [(number? primary-orbit)
         (number? orbit)]}
  (or (<= orbit (/ primary-orbit 2))
      (>= orbit (* primary-orbit 2))))

(defn orbits-allowed-for-primaries [max-orbit primary-orbits]
  (for [o (range (inc max-orbit))
        :when (reduce (fn [acc po]
                        (and acc
                             (or (= po 'Close)
                                 (= po 'Far)
                                 (orbit-allowed-for-primary po o))))
                      true
                      primary-orbits)]
    o))

(defn set-orbits [stars]
  (let [primary (first (filter :is-primary? stars))
        secondary-orbits (->> stars
                              (remove :is-primary?)
                              (map :orbit))]
    (for [{:keys [size type_ subtype] :as s} stars
          :let [max-orbit (max-star-orbit s)]]
      (let [orbits (if (:is-primary? s)
                     (orbits-allowed-for-primaries max-orbit
                                                   secondary-orbits)
                     (range (inc max-orbit)))]
        (assoc s :orbits (for [o orbits]
                           {:num o
                            :zone (lookup-zone size type_ subtype o)}))))))

(defn set-empty-orbits [{:keys [type_ orbits] :as star}]
  (let [dm (if (#{'A 'B} type_) 1 0)
        has-empty? (>= (+ (d 1) dm) 5)
        empty-orbits-table {1 1, 2 1, 3 2, 4 3, 5 3, 6 3, 7 3}
        num-empty (if has-empty?
                    (min (count orbits)
                         (empty-orbits-table (+ (d 1) dm)))
                    0)
        empty-orbits (loop [empties #{},  n 0]
                       (if (= n num-empty)
                         empties
                         (let [candidate-orbit (- (d) 2)]
                           (if (or (empties candidate-orbit)
                                   (> candidate-orbit
                                      (apply max (map :num orbits))))
                             (recur empties n)
                             (recur (conj empties candidate-orbit)
                                    (inc n))))))]
    (assoc star :orbits
           (for [o orbits]
             (assoc o :empty? (boolean (empty-orbits (:num o))))))))

(defn set-captured-planets [{:keys [size type_ subtype orbits] :as star}]
  (let [dm (if (#{'A 'B} type_) 1 0)
        has-captured? (>= (+ (d 1) dm) 5)
        captured-planets-table {1 1, 2 1, 3 2, 4 2, 5 3, 6 3, 7 3}
        num-captured (if has-captured?
                       (captured-planets-table (+ (d 1) dm))
                       0)
        capture-orbits (for [_ (range num-captured)
                             :let [o-num (+ (- (d) 7) (* 0.1 (- (d) 7)))
                                   io-num (int o-num)
                                   o-num (if (= (float io-num)
                                                o-num)
                                           io-num o-num)]
                             :when (and (> o-num 0)
                                        (not (some #{o-num}
                                                   (map :num orbits))))]
                         {:num o-num
                          :zone (lookup-zone size type_ subtype io-num)
                          :empty? false
                          :captured? true})]
    (assoc star :orbits
           (sort-by :num (concat orbits capture-orbits)))))

(defn place-ggs [{:keys [orbits] :as star}]
  (let [gg-present? (= 'yes (t/gas-giant-present (d)))
        num-ggs (t/gas-giant-qty (d))
        max-orbit-num (some->> orbits
                               last
                               :num
                               int
                               inc)
        extra-orbit {:num max-orbit-num
                     ;; FIXME: this orbit num might not be outer:
                     :zone :outer
                     :empty? false}
        candidate-orbit-fn (fn [{:keys [zone empty?]}]
                             (and (not empty?) (#{:outer :habitable} zone)))
        num-gg-orbit-candidates (count (filter candidate-orbit-fn orbits))
        new-orbits (if (> num-ggs num-gg-orbit-candidates)
                     (concat orbits [extra-orbit])
                     orbits)
        new-orbits-count (count new-orbits)
        orbits-with-ggs
        (loop [n (min num-ggs new-orbits-count)
               orbits (vec new-orbits)]
          (let [which-orbit (rand-int new-orbits-count)
                o (nth orbits which-orbit)]
            (cond
              (empty? orbits) orbits
              (zero? n) orbits
              (not (candidate-orbit-fn o)) (recur n orbits)
              :else
              (recur (dec n) (update orbits which-orbit
                                     assoc :type_ :gg,
                                     :size (rand-nth [:large :small]))))))]
    (assoc star :orbits orbits-with-ggs)))

(defn gen-system []
  ;;                               CHECKLIST:
  (->> (stars)                     ;; Steps 2 A,B,C
       (map name-system)           ;;
       set-companion-orbits        ;; 2 D
       set-orbits                  ;; 2 E,F
       (map set-empty-orbits)      ;; 2 G
       (map set-captured-planets)  ;; 2 G
       (map place-ggs)             ;; 2 H
       ))

#_(dotimes [_ 100] (with-out-str (pr (gen-system))))

#_(->> gen-system
       (repeatedly 3000)
       (mapcat (partial map :orbits))
       (map (partial filter :type_))
       (map count)
       frequencies)
;;=>
'{:inner 599, :habitable 182, :outer 1888, :inside-star 63, :scorched 25}

(gen-system)
