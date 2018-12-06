(ns day3.core
  (:require [aoc.core :as aoc] [clojure.string :as str]
            [clojure.set :as set]))

(def d
  (map (fn [l]
         (map #(Integer. %) (rest (re-find #"(\d+),(\d+):\s+(\d+)x(\d+)" l))))
       (aoc/puzzle-lines "day3")))

(defn extent [x y w h]
  (for [b (range y (+ y h)) a (range x (+ x w))] [a b]))

(defn draw [grid coords claim]
  (into grid (for [c coords] [c (conj (get grid c []) claim)])))

(defn draw-all [data]
  (loop [t data g {} claim 1]
    (if (empty? t)
      g
      (recur (rest t) (draw g (apply extent (first t)) claim) (inc claim)))))

(defn part1 []
  (count (filter #(> (count %) 1) (vals (draw-all d)))))

;;; make a map of how many concurrent claims there are with a
;;; particular claim. then invert it, and find the claim corresponding
;;; to 1
(defn part2 []
  (get (set/map-invert
        (loop [claims (vals (draw-all d)) counts {}]
    (if (empty? claims)
      counts
      (recur
       (rest cls)
       (into counts (for [c (first claims)]
                      [c (max (get counts c 0) (count (first claims)))]))))))
       1))
