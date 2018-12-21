(ns day11.core
  (:require [aoc.core :as aoc] [clojure.string :as str]
            [clojure.set :as set]))

(def power
  (memoize
   (fn [x y srl]
     (let [rack-id (+ 10 x)
           p1 (* rack-id (+ srl (* rack-id y)))
           p2 (mod (int (/ p1 100)) 10)] (- p2 5)))))

(defn cluster [x y z srl]
  (for [y (range y (+ z y))]
    (for [x (range x (+ z x))]
      (power x y srl))))

(defn score3 [[x y] srl]
  (reduce + (flatten (cluster x y 3 srl))))

(defn score [[x y z] srl]
  (reduce + (flatten (cluster x y z srl))))

(defn part1 [srl]
  (let [g (apply concat
                 (for [y (range 1 298)]
                   (for [x (range 1 298)] [x y])))
        s (into {} (zipmap g (map #(score3 % srl) g)))]
    (first (apply max-key val s))))

;; find the best square size for any x y pair
;; limited to 3-20
(defn best-score [[x y] srl]
  (let [max-size (min 20 (- 301 (max x y)))
        g (for [z (range 3 (inc max-size))] [x y z])
        s (into {} (zipmap g (map #(score % srl) g)))]
    (apply max-key val s)))

;; I decided only to search squares 3-20 in dimension
;; this got me the right answer, so I didn't need to go further
(defn part2 [srl]
  (loop [x 1 y 1 max-score 0 max-coords nil]
    (if (and (= x 1) (= 0 (mod y 10))) (println x y))
    (if (= x 298)               ; end of row, move to next row or terminate
      (if (= y 298)             ; end of the road
        [max-score max-coords]
        (recur 1 (inc y) max-score max-coords)) ; else bump y, loop
      ;; not at end of row, so we calculate
      (let [[c s] (best-score [x y] srl)
            nm (if (< max-score s) s max-score)
            nc (if (< max-score s) c max-coords)]
        (recur (inc x) y nm nc)))))

;; sneaking suspicion there are insights that could really cut
;; down the search space....

