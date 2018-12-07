(ns day5.core
  (:require [aoc.core :as aoc] [clojure.string :as str]
            [clojure.set :as set]))

(def inp (first (aoc/puzzle-lines "day5")))

(defn ok-pair? [a b]
  (or (nil? a) (nil? b)
      (not (= 32 (Math/abs (- (int a) (int b)))))))

(defn process [d]
  "Treat problem as a stack, popping off a destroyed polymer"
  (loop [ps (vec d) op (list)]
    (if (empty? ps) op
        (if (ok-pair? (first op) (first ps))
          (recur (rest ps) (cons (first ps) op))
          (recur (rest ps) (rest op))))))

(defn part1 [d] (count (process d)))

(def alphabet [\A \B \C \D \E \F \G \H \I \J \K \L \M
               \N \O \P \Q \R \S \T \U \V \W \X \Y \Z])

(defn remove-polymer [t v]
  (remove #{t (char (+ 32 (int t)))} v))

(defn part2 [d]
  (->> alphabet
       (map #(part1 (remove-polymer % d)))
       (sort)
       (first)))

;; (part1 inp) -> 11540
;; (part2 inp) -> 6918
