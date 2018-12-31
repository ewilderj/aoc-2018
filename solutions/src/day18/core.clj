(ns day18.core
  (:require [aoc.core :as aoc] [clojure.string :as str]))

(def adjacencies [[-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1]])
(defn v+ [a b] (mapv + a b))
(defn scan [m p] (map (partial get m) (mapv (partial v+ p) adjacencies)))
(defn counts [m p] (frequencies (filter identity (scan m p))))

(def inp (aoc/puzzle-lines "day18"))
(def exinp (aoc/puzzle-lines "day18-ex"))

(defn parse-line [l y] (map-indexed #(vector [% y] %2) l))
(defn make-maze [i]
  (->> i
       (map-indexed #(parse-line %2 %))
       (apply concat)
       (into {})))

(defn decide [m p]
  (let [c (counts m p) s (m p)]
    (cond
      (and (= s \.) (>= (c \| 0) 3)) \|
      (and (= s \|) (>= (c \# 0) 3)) \#
      (= s \#) (if (and (> (c \# 0) 0) (> (c \| 0) 0)) \# \.)
      :else s)))

(defn turn [m]
  (reduce (fn [r p] (assoc r p (decide m p))) {} (keys m)))

(defn ms
  "Debug function to turn map into string"
  [m]
  (let [mx (apply max (map first (keys m)))
        my (apply max (map second (keys m)))]
    (str/join "\n" (for [y (range 0 (inc my))]
                     (apply str (for [x (range 0 (inc mx))] (m [x y])))))))

(defn score [m]
  (apply * (map (frequencies (vals m)) [\| \#])))

(defn part1 [i]  ;; => 467819
  (score (nth (iterate turn (make-maze i)) 10)))

(defn find-cycle [results]
  (loop [i 550 seen {}]      ;; doesn't cycle til at least 550, set to 0 for new data
    (let [r (nth results i)]
      (if (seen r)
        [(seen r) i]
        (recur (inc i) (assoc seen r i))))))

(defn part2 [i]  ;; => 195305
  (let [m (make-maze i)
        results (iterate turn m)
        [x y] (find-cycle results)]
    (score (nth results (+ x (mod (- 1000000000 x) (- y x)))))))
