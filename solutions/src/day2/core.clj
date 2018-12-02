(ns day2.core
  (:require [aoc.core :as aoc] [clojure.string :as str]
            [clojure.set :as set]))

(def d (aoc/puzzle-lines "day2"))

(def part1
  (let [g (fn [l] (keys (set/map-invert (frequencies l))))
        f (frequencies (flatten (map g d)))]
    (* (f 2) (f 3)))) ; 8715

;; part 2
;;
;; make tuple pairs of positions, locate only those pairs with one
;; difference.
(defn positions [s] (set (partition 2 (interleave (range) s))))

(def perms [l]
  (loop [l l a []]
    (let [m (first l) n (rest l)]
      (if (empty? n) a
          (recur n (concat a (partition 2 (interleave (repeat m) n))))))))

(defn diffs [[x y]] (count (set/difference (positions x) (positions y))))

(defn find-answer [l]
  (let [p (perms l)
        [c1 c2] (second (first (filter (fn [[x y]] (= x 1))
                                       (partition 2 (interleave (map diffs p) p)))))
        c (first (set/difference (set (vec c1)) (set (vec c2))))]
    (str/replace c1 (str c) "")))

(def part2 (find-answer d)) ; fvstwblgqkhpuixdrnevmaycd
