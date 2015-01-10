(ns trav.util)


(def ^:private base-ord
  (- (int (.charAt "A" 0)) 10))


(defn hexish-code [x]
  {:pre [(> x -1), (< x 36)]}
  (if (< x 10)
    (str x)
    (->> x
         (+ base-ord)
         char
         str)))


(defn take-until [f [x & xs]]
  (lazy-seq
   (cons x (if (or (f x) (nil? xs))
             nil
             (take-until f xs)))))


(defn keywordize [s]
  (-> s
      name
      (#(.toLowerCase %))
      keyword))
