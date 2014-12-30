(ns trav.util)


(defn hexcode [x]
  {:pre [(< x 16), (> x -1)]}
  (format "%X" x))


(defn take-until [f [x & xs]]
  (lazy-seq
   (cons x (if (or (f x) (nil? xs))
             nil
             (take-until f xs)))))
