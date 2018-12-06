(ns day5.core
  (:require [aoc.core :as aoc] [clojure.string :as str]
            [clojure.set :as set]))

(def inp (first (aoc/puzzle-lines "day5")))

(defn ok-pair? [a b]
  (not (= 32 (Math/abs (- (int a) (int b))))))

(defn process [d]
  (loop [l (vec d) nl []]
    (if (empty? (rest l)) (concat nl l)
        (if (ok-pair? (first l) (second l))
          (recur (rest l) (conj nl (first l)))
          (recur (drop 2 l) nl)
          ))))

(defn part1 [d]
  (count (apply str (loop [od d]
    (let [nd (process od)]
      (if (= od nd) od (recur nd)))))))

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
