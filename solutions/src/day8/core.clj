(ns day8.core
  (:require [aoc.core :as aoc] [clojure.string :as str]
            [clojure.set :as set]))

(def t [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2])

(def inp (map #(Integer. %) (str/split (aoc/puzzle-data "day8") #"\s+")))

(defn build-tree [v])


;; builds a tree that looks like a node followed by tuples
;; for each children, where the tuples contain metadata and
;; their children. e.g. for the example given in the problem
;; it looks like: [(1 1 2) ((10 11 12) [] (2) ((99) []))]]

;; aux function that uses mutual recursion to process children
(defn process-kids [kids tape]
  (loop [kids kids tape tape m []]
    (if (= 0 kids) [tape m]                          ; if no kids, return metadata
        (let [[ntape nm] (build-tree tape)]          ; process child node
          (recur (dec kids) ntape (concat m nm)))))) ; concatenate nested metadata

(defn build-tree [v]
  (if (some? v)
    (let [mn (second v)                                 ; no. of metadata
          [nv res] (process-kids (first v) (drop 2 v))] ; process our children
      [(drop mn nv) [(take mn nv) res]])                ; ret [inp [metadata, child-metadata]]
    [[] []]))                                           ; base case, nothing to do


;; part1 - we don't need structure, just add the numbers
(defn part1 [v]
  (reduce + (flatten (second (build-tree v)))))

;; part 2 - compute totals according to rules
(defn compute [[md kids]]
  (if (empty? kids)
    (reduce + md)                                    ; no kids, just add up
    (let [res (map compute (partition 2 kids))       ; (kids, value) tuples
          ; prepare metadata indices by dropping invalid, and moving to 0-indexed
          nmd (map dec (filter #(or (= 0 %) (<= % (count res))) md))]

      (reduce + (map #(nth res %) nmd))))) ; add up values of children

(defn part2 [v]
  (compute (second (build-tree v))))
