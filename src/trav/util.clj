(ns trav.util)


(defn hexcode [x]
  {:pre [(< x 16), (> x -1)]}
  (format "%X" x))
