(ns day6.core
  (:require [aoc.core :as aoc] [clojure.string :as str]
            [clojure.set :as set]))

(def test-data [[1 1] [1 6] [8 3] [3 4] [5 5] [8 9]])

(def inp (for [l (aoc/puzzle-lines "day6")]
           (map #(Integer. %) (str/split l #",\s+"))))

(defn bounding [d]
  (let [x1 (apply min (map first d))
        x2 (apply max (map first d))
        y1 (apply min (map second d))
        y2 (apply max (map second d))]
    [[x1 y1] [x2 y2]]))

(defn mhd [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x2 x1)) (Math/abs (- y2 y1))))

(defn closest-or-tie [distances]
  (if (= (second (second distances)) (second (first distances)))
    nil (first (first distances))))

(defn nearest [x y d]
  (->> d
       (map #(mhd [x y] %))
       (interleave (range))
       (partition 2)
       (sort-by second)
       (closest-or-tie)))

(defn disqualify? [[[x1 y1] [x2 y2]] [x y]]
  (or (contains? #{x1 x2} x)
      (contains? #{y1 y2} y)))

(defn not-infinite [d]
  (let [b (bounding d)]
  (->> d
       (interleave (range))
       (partition 2)
       (filter #(not (disqualify? b (second %))))
       (map first))))

(defn explore [d]
  (let [[[x1 y1] [x2 y2]] (bounding d)]
    (for [x (range x1 (inc x2))]
      (for [y (range y1 (inc y2))]
        [[x y] (nearest x y d)]))))

(defn part1 [d]
  (let [f (->> d
               (explore)
               (apply concat)
               (map second)
               (frequencies))]
    (apply max (map f (not-infinite d)))))

(defn sumdist [x y d m]
  (< (->> d
       (map #(mhd [x y] %))
       (reduce +)) m))

(defn explore2 [m d]
  (let [[[x1 y1] [x2 y2]] (bounding d)]
    (for [x (range x1 (inc x2))]
      (for [y (range y1 (inc y2))]
        [[x y] (sumdist x y d m)]))))

(defn part2 [d m]
  (->> d
       (explore2 m)
       (apply concat)
       (map second)
       (filter identity)
       (count)))

;; (part1 inp) -> 5365  ;; very slow in my implementation
;; (part2 inp 10000) -> 42513 ;; also very slow

