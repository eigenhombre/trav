(ns trav.macros
  "
  Macro definitions, mostly for defining tables for the purposes of
  making them look like the LBBs.  Normally this would be a lot of
  macro sugar for this sort of project, but one goal I've had is to
  make it easy to go back and forth between the rule books and the
  code and see how various things are similarly defined.
  "
  (:require [trav.util :refer [keywordize]]))


(defmacro defcoll [name & syms]
  `(def ~name (map keywordize (quote ~syms))))


(defn row-vec [[service base-roll dms]]
  [(keyword service) {:base-roll (if (= base-roll '-)
                                   Double/POSITIVE_INFINITY
                                   base-roll)
                      :dms (map (fn [[attr thresh _ dm]]
                                  {:attr (keywordize attr)
                                   :thresh thresh
                                   :dm dm})
                                (partition 4 dms))}])


(defmacro def-service-table [tname & service-rows]
  `(do (def ~tname
         (->> (quote ~(partition 3 service-rows))
              (mapcat row-vec)
              (apply hash-map)))
       ~tname))


(defn nil-if-dash [symb] (if (= symb '-) nil symb))


(defn selection-row-vec [[svc & elts]]
  [(keyword svc) (map nil-if-dash elts)])


(defmacro def-selection-table [tname & rows]
  `(do (def ~tname
         (->> '~rows
              (partition 7)
              (mapcat selection-row-vec)
              (apply hash-map)))
       ~tname))


(defmacro def-aging-table [tname & rows]
  `(do (def ~tname
         (->> '~rows
              (partition-by symbol?)
              (apply concat)
              (map (fn [s#] (if (symbol? s#) (keywordize s#) s#)))
              (apply hash-map)))
       ~tname))


(defmacro def-rnd-selection-table [tname & data]
  `(do
     (def ~tname
       (let [svcs# (map keywordize (take 6 '~data))
             rows# (->> '~data
                        (drop 6)
                        (map nil-if-dash)
                        (partition 7)
                        (map (comp (partial map vector) rest)))]
         (->> rows#
              (map (partial interleave svcs#))
              (map (partial apply hash-map))
              (apply (partial merge-with (comp vec concat))))))
     ~tname))


(defn- expand-sym [sym]
  (if-let [[_ s e] (re-find #"r(\d+)-(\d+)" (str sym))]
    [(Integer. s) (Integer. e)]
    [sym sym]))


(defn expand [[sym val]]
  (let [[start end] (expand-sym sym)]
    (apply hash-map
           (interleave (range start (inc end))
                       (repeat val)))))

(defmacro def-range-table [tname & body]
  `(do (def ~tname (->> '~body
                        (partition 2)
                        (map expand)
                        (apply merge)))
       ~tname))


(defmacro evalq
  "
  Macro for interpolating an expression inline in source
  code (e.g. for Marginalia).
  "
  [expr]
  ``(quote ~~expr))
