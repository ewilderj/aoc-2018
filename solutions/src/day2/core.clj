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

(defn pairwise-perms [l]
  (loop [l l a []]
    (let [m (first l) n (rest l)]
      (if (empty? n) a
          (recur n (concat a (partition 2 (interleave (repeat m) n))))))))

(defn positions [s] (set (map-indexed list s)))
(defn diff-count [[x y]] (count (set/difference (positions x) (positions y))))

(defn find-answer [l]
  (let [p (pairwise-perms l)                  ; every code combo
        s ((zipmap (map diff-count p) p) 1)   ; the pair with one diff
        c (apply set/difference (map set s))] ; the differing char
    (apply str (remove c (first s)))))        ; remove it from the code

(def part2 (find-answer d)) ; fvstwblgqkhpuixdrnevmaycd

;;; this is a fast version that uses lazy evaluation, and
;;; stops searching when we find the first pair of codes
;;; that's got only one difference
;;; 2.9 seconds vs 4.3 seconds for the original

(defn faster-answer [l]
  (let [s (->> l
               (pairwise-perms)               ; every code combo
               (#(interleave (map diff-count %) %))
               (partition 2)
               (filter (fn [[n p]] (= n 1)))
               (first)
               (last))                        ; the pair with one diff
        c (apply set/difference (map set s))] ; the differing char
    (apply str (remove c (first s)))))        ; remove it from the code


