(ns trav.dice)


(defn d
  ([] (d 2))
  ([n] (reduce + (repeatedly n #(inc (rand-int 6))))))
