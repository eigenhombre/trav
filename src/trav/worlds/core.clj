(ns trav.worlds.core
  (:require [namejen.names :refer [generic-name]]
            [trav.dice :refer [d]]
            [trav.worlds.tables :as t]
            [clojure.string :as str]))

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

(defn name-star [sys]
  (assoc sys :name_ (generic-name)))

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

(defn set-empty-orbits [{:keys [type_ orbits zone] :as star}]
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
           (for [{:keys [zone] :as o} orbits]
             (assoc o :empty? (or (= zone :inside-star)
                                  (= zone :scorched)
                                  (boolean (empty-orbits (:num o)))))))))

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
                                           io-num o-num)
                                   zone (lookup-zone size type_ subtype io-num)]
                             :when (and (> o-num 0)
                                        (not (some #{o-num}
                                                   (map :num orbits)))
                                        (not (= :inside-star zone))
                                        (not (= :scorched zone)))]
                         {:num o-num
                          :zone zone
                          :empty? false
                          :captured? true})]
    (assoc star :orbits
           (sort-by :num (concat orbits capture-orbits)))))

(defn world-name []
  (case (rand-int 40)
    0 (str (generic-name) "'s World")
    1 (str (rand-nth ["The "
                      "The "
                      "The "
                      "The "
                      "Le "
                      "La "
                      "Il "
                      "l'"
                      "Der "
                      "Den "
                      "Das "
                      "Ul "  ;; Bezel
                      ]) (generic-name))
    (let [n (-> 5
                rand-int
                rand-int
                rand-int
                inc)]
      (str/join " " (repeatedly n generic-name)))))

;; FIXME: GGs CAN be in the inner zones if habitable and outer orbits
;; are filled.
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
                                     :name_ (world-name)
                                     :size (rand-nth [:large :small]))))))]
    (assoc star :orbits orbits-with-ggs)))

(defn population [zone atmosphere]
  (let [pop-dm (+ (if (= zone :inner) -5 0)
                  (if (= zone :outer) -3 0)
                  (if (not (#{0 5 6 8} atmosphere)) -2 0))]
    (max 0 (+ (d) -2 pop-dm))))

(defn place-planetoids [{:keys [orbits] :as star}]
  (let [nonempty-orbits (remove :empty? orbits)
        is-gg? (comp (partial = :gg) :type_)
        gg-orbits (filter is-gg? nonempty-orbits)
        available-orbits (remove is-gg? nonempty-orbits)
        num-ggs (count gg-orbits)
        present? (= 'yes (t/planetoid-present (- (d) num-ggs)))
        num-planetoids (min (count available-orbits)
                            (or (t/planetoid-qty (- (d) 0 num-ggs )) 0))
        shuffled (shuffle orbits)
        planetoid-orbits (->> shuffled
                              (take num-planetoids)
                              (map (fn [m] (assoc m
                                                  :type_ :planetoid
                                                  :name_ "Planetoid belt"
                                                  :atmosphere 0
                                                  :hydrographics 0
                                                  :population (population
                                                               (:zone m)
                                                               0)
                                                  :size 0))))
        non-planetoid-orbits (drop num-planetoids shuffled)]
    (assoc star :orbits (sort-by :num (concat planetoid-orbits
                                              non-planetoid-orbits)))))

(defn generate-world [star-type orbit-num zone]
  (let [orbit-size-dm (get {0 -5
                            1 -4
                            2 -2} orbit-num 0)
        type-m-dm (if (= star-type 'M) -2 0)
        size (max 0 (+ (d) -2 orbit-size-dm type-m-dm))
        atmo-dm (case zone
                  :inner -2
                  :outer -4
                  0)
        atmo (cond
               (zero? size) 0
               ;; The rules have something goofy about two orbits away
               ;; from the habitable zone, but I think that's a little
               ;; crazy, so just pick Exotic atmo one out of 36 times.
               (= (d) 12) 10
               :else (if (zero? size) 0 (max 0 (+ (d) -7 size atmo-dm))))
        hydro-dm (+ (if (= zone :outer) -2 0)
                    (if (or (< atmo 2) (> atmo 9)) -4 0))
        hydro (cond
                (zero? size) 0
                (= zone :inner) 0
                :else (-> (d)
                          (+ -7 size hydro-dm)
                          (max 0)
                          (min 10)))]
    {:type_ :planet
     :name_ (world-name)
     :size size
     :atmosphere atmo
     :hydrographics hydro
     :population (population zone atmo)}))

(defn place-worlds [star]
  (assoc star
         :orbits
         (for [{:keys [empty? zone type_ num] :as o} (:orbits star)]
           (if (and (not empty?)
                    (not type_)
                    (not (= zone :scorched))
                    (not (= zone :inside-star)))
             (merge o (generate-world (:type_ star) num zone))
             o))))

(defn determine-main-world [system]
  ;; FIXME: consider moons as well
  (let [[_ _ _ i j]
        (->> (for [[i star] (map-indexed vector system)
                   [j orbit] (map-indexed vector (:orbits star))]
               [(or (:population orbit) 0) (:zone orbit) (:num orbit) i j])
             (sort-by (juxt (comp - first)
                            second
                            (comp - #(or (nth % 2) 0))))
             first)]
    (if (and i j)
      (update-in (vec system) [i :orbits]
                 (comp #(assoc-in % [j :is-main-world?] true) vec))
      system)))

(defn determine-main-world-attribs [star]
  (update star :orbits
          (fn [orbits]
            (map (fn [{:keys [is-main-world? population] :as o}]
                   (if is-main-world?
                     (let [govt (max 0 (+ (d) -7 (or population 0)))
                           ll (max 0 (+ (d) -7 govt))]
                       (assoc o
                              :government govt
                              :law-level ll
                              :starport (t/starports (d))))
                     ;; FIXME: Handle non main-worlds
                     (assoc o
                            :government 0
                            :law-level 0
                            :starport 'Y)))
                 orbits))))

(defn satellite-size [{:keys [size type_] :as w}]
  (let [size-num
        (if (= type_ :gg)
          (if (= size :small)
            (- (d) 6)
            (- (d) 4))
          (- size (d 1)))]
    (cond
      (neg? size-num) :small
      (zero? size-num) :ring
      :else size-num)))

(def satellite-orbits
  (let [cols [#_0 :close :far :extreme
              2   3       15  75
              3   4       20  100
              4   5       25  125
              5   6       30  150
              6   7       35  175
              7   8       40  200
              8   9       45  225
              9  10       50  250
              10 11       55  275
              11 12       60  300
              12 13       65  325]
        col-names (take 3 cols)]
    (into {}
          (apply concat
                 (for [[roll c f e] (partition 4 (drop 3 cols))]
                   [[[:close roll] c]
                    [[:far roll] f]
                    [[:extreme roll] e]])))))

(defn ring-orbit []
  (condp = (d 1)
    1 1
    2 1
    3 1
    4 2
    5 2
    6 3))

(defn satellite-orbit [{:keys [type_] :as world} index sat-size]
  (if (= sat-size :ring)
    (ring-orbit)
    (let [_ (assert (or (= sat-size :small)
                        (number? sat-size)))
          roll (d)
          orbit-type (cond
                       (and (= type_ :gg) (= roll 12)) :extreme
                       (> roll 7) :far
                       :else :close)
          roll-w-dm (->> index
                         (- roll)
                         (max 2)
                         (min 12))]
      (satellite-orbits [orbit-type roll-w-dm]))))

(defn gen-satellites-for-world [{:keys [size type_] :as world}]
  (let [num-sats
        (max 0 (cond
                 (not size) 0
                 (= type_ :planetoid) 0
                 (= type_ :planet) (if (zero? size) 0 (- (d 1) 3))
                 (= type_ :gg)
                 (- (d) (if (= size :large) 0 4))))
        satellites
        (loop [index 0
               orbits #{}
               ret []]
          (if (>= index (dec num-sats))
            (sort-by :num ret)
            (let [sat-size (satellite-size world)
                  orbit (satellite-orbit world index sat-size)]
              (if (orbits orbit)
                (recur index orbits ret)
                (recur (inc index)
                       (conj orbits orbit)
                       (conj ret {:type_ :satellite
                                  :size sat-size
                                  :num orbit
                                  :name_ (world-name)}))))))]
    (assoc world :satellites satellites)))

(defn gen-satellites-for-star [star]
  (update star :orbits (partial map gen-satellites-for-world)))

(defn gen-system []
  ;;                                       CHECKLIST:
  (->> (stars)                             ;; Steps 2 A,B,C
       (map name-star)                     ;;
       set-companion-orbits                ;; 2 D
       set-orbits                          ;; 2 E,F
       (map set-empty-orbits)              ;; 2 G
       (map set-captured-planets)          ;; 2 G
       (map place-ggs)                     ;; 2 H, 3 A
       (map place-planetoids)              ;; 2 I, 3 B
       (map place-worlds)                  ;; 4 A B
       determine-main-world                ;; 7
       (map determine-main-world-attribs)  ;; 7 A-G
       (map gen-satellites-for-star)
       ))

(defn- many-orbits
  "
  For testing
  "
  [n]
  (->> gen-system
       (repeatedly n)
       (mapcat (partial mapcat :orbits))))

(->> 10
     many-orbits
     (map :population)
     sort
     set)
;;=>
'#{nil 0 7 1 4 6 3 2 5 8}

(defn to-hex [n] (get {10 "A" 11 "B" 12 "C" 13 "D" 14 "E" 15 "F"} n n))

(defn size-code [{:keys [size type_] :as w}]
  (case type_
    :planet (if (= size 0)
              "S"
              (to-hex size))
    ;; FIXME: Ugly w.r.t prev lines:
    :satellite (cond
                 (= size :ring) "R"
                 (= size :small) "S"
                 :else (to-hex size))
    :planetoid "0"))

(defn upp [{:keys [starport size atmosphere hydrographics
                   type_ population government law-level] :as w}]
  (if (= type_ :gg)
    (str (clojure.string/capitalize (name size)) " GG")
    (str starport
         (size-code w)
         (to-hex atmosphere)
         (to-hex hydrographics)
         (to-hex population)
         (to-hex government)
         (to-hex law-level))))

(defn table-str
  "
  (table-str [[1 2 33], [666 444 2]])
  ;;=>
  \"  1   2 33
  666 444  2\"
  "
  [rows]
  (let [col-lengths (for [col (range (count (first rows)))]
                      (->> rows
                           (map (comp count str #(nth % col)))
                           (apply max)))
        fmt-str (clojure.string/join
                 (for [cl col-lengths]
                   (if (zero? cl)
                     "%1s"
                     (str " %" cl "s"))))]
    (->> rows
         (map (partial apply format fmt-str))
         (map clojure.string/trimr)
         (clojure.string/join "\n"))))

(defn system-str [system]
  (table-str
   (cons ["Orbit" "" "Name" "UPP" "" "" "Remarks"]
         (apply concat
                (for [{:keys [is-primary? empty? type_ size
                              subtype name_ orbits]}
                      system]
                  (cons
                   [(if is-primary?
                      "Primary"
                      "Companion")
                    ""
                    name_
                    (str type_ subtype " " size)
                    "" "" ""]
                   (apply concat
                          (for [{:keys [num name_ empty?
                                        is-main-world? satellites] :as o} orbits
                                :when (not empty?)]
                            (cons
                             [(str
                               (if is-main-world? "*" "")
                               (if (double? num)
                                 (format "%.1f" num)
                                 (str num)))
                              "" (or name_ "") (upp o) "" "" ""]
                             (for [{:keys [num name_ size] :as sat} satellites]
                               ["" num name_ (upp sat) "" "" ""]))))))))))

(comment)
(dotimes [_ 5]
  (println "_____________________")
  (println (system-str (gen-system))))
