(ns trav.core)


(defn d
  ([] (d 1))
  ([n] (loop [n n, ret 0]
         (if (= n 0)
           ret
           (recur (dec n) (+ ret (inc (rand-int 6))))))))


(def attributes [:st :dx :en :in :ed :ss])


(defn char-attr-map []
  (zipmap attributes (take (count attributes)
                           (repeatedly #(d 2)))))


(defn hc [x]
  {:pre [(< x 16)
         (> x -1)]}
  (format "%X" x))


(defn hstr [l]
  (apply str (map hc l)))


(defn string-for-attrs [m]
  (hstr (for [a attributes] (m a))))


(for [_ (range 100)]
  (string-for-attrs (char-attr-map)))
