(ns trav.core)


(defn d
  ([] (d 2))
  ([n] (reduce + (repeatedly n #(inc (rand-int 6))))))


(def attributes '(ST DX EN IN ED SS))


(defn char-attr-map []
  (zipmap attributes (take (count attributes)
                           (repeatedly d))))


(defn hc [x]
  {:pre [(< x 16), (> x -1)]}
  (format "%X" x))


(defn hstr [l]
  (apply str (map hc l)))


(defn upp [m]
  (hstr (map m attributes)))


(comment

(take 10 (repeatedly #(upp (char-attr-map))))

;;=>
("928B73"
 "568678"
 "59546A"
 "7C8B88"
 "A756A8"
 "86A667"
 "834699"
 "634732"
 "973827"
 "455C4C")

)

(def services [:navy :marines :army :scouts :merchants :other])

(def enlist-dms
  {:navy
   {:base-roll 8
    :dms {:in [8 1]
          :ed [9 2]}},
   :marines
   {:base-roll 9
    :dms {:in [8 1]
          :st [8 2]}},
   :army
   {:base-roll 5
    :dms {:dx [6 1]
          :en [5 2]}},
   :scouts
   {:base-roll 7
    :dms {:in [6 1]
          :st [8 2]}},
   :merchants
   {:base-roll 7
    :dms {:st [7 1]
          :in [6 2]}},
   :other
   {:base-roll 3
    :dms {}}})


(defn apply-enlistment-dms [dms attrs]
  (apply +
         (for [[attr [min bonus]] dms :when (>= (attrs attr) min)]
           bonus)))

(defn select-service [attrs]
  "Select desired service at random, try to enlist based on DMs, and
   select based on 'draft' otherwise."
  (let [desired (pick services)
        {base :base-roll, dms :dms} (enlist-dms desired)
        dm (apply-enlistment-dms dms attrs)
        success (>= (+ (d) dm) base)]
    (if success desired (pick services))))


(let [namer (name-maker 4 (get-default-name-data))]
  (doseq [_ (range 10)]
    (let [name (namer)
          attrs (char-attr-map)
          service (select-service attrs)]
      (println (upp attrs)
               name
               service))))
