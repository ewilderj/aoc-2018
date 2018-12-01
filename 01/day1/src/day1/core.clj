(ns day1.core
  (:require [clojure.string :as str]))

(def n (map #(Integer. %)
            (str/split-lines (slurp "/home/edd/work/github/aoc-2018/01/day1/puzzle.txt"))))

(def part1 (reduce + n)) ; 520

(defn first-dup [c]
  (loop [seen #{} c c]
    (if (empty? c)
      nil
      (let [v (first c)]
        (if (contains? seen v) v (recur (conj seen v) (rest c)))))))

(def part2 (first-dup (reductions + (flatten (repeat n)))))

;;; lazy way of doing things, more unreadable, but could extend
;;; to find the nth solution easily, by filtering for the final
;;; tuple of the iteration to have length n

(defn seen-step [[l s r]]
  (let [v (first l)]
    (if (contains? s v)
      (list (rest l) s (conj r v))
      (list (rest l) (conj s v) r)
  )))

(def part2-lazy
  (first (last (first
                (filter #(seq (last %))
                        (iterate seen-step [(reductions + (flatten (repeat n))) #{} []]))))))
