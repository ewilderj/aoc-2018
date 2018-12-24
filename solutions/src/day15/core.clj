(ns day15.core
  (:require [aoc.core :as aoc] [clojure.string :as str]
            [clojure.set :as set] [clojure.spec.alpha :as s]
            [clojure.data.priority-map :refer [priority-map]]))

(def inp (aoc/puzzle-lines "day15-ex1"))

(defn map-vals [m f]
  (into {} (for [[k v] m] [k (f v)])))

(defn remove-keys [m pred]
  (select-keys m (filter (complement pred) (keys m))))

(defn dijkstra
  "Computes single-source shortest path distances in a directed graph.

  Given a node n, (f n) should return a map with the successors of n
  as keys and their (non-negative) distance from n as vals.

  Returns a map from nodes to their distance from start."
  [start f]
  (loop [q (priority-map start 0) r {}]
    (if-let [[v d] (peek q)]
      (let [dist (-> (f v) (remove-keys r) (map-vals (partial + d)))]
        (recur (merge-with min (pop q) dist) (assoc r v d)))
      r)))

(defn parse-line
  "Read a line of maze, and remove creatures into a separate list"
  [y l]
  (loop [l l creatures [] r [] x 0]
    (if (empty? l) [r creatures]
        (let [c (first l)]
          (if (#{\G \E} c) 
            (recur (rest l) (conj creatures [[x y] c]) (conj r \.) (inc x))
            (recur (rest l) creatures (conj r c) (inc x)))))))

(defn universe
  "A map used for game state"
  [maze creatures] {:maze maze :creatures creatures})

(defn parse-input
  "Process all puzzle lines from a vector of lines"
  [i]
  (let [s (map-indexed parse-line i)]
    (universe (vec (map first s))
              (apply concat (map second s)))))

(defn creature-at?
  "In universe u, is there a creature at [x y]?"
  [u [x y]]
  (contains? (set (map first (u :creatures))) [x y]))

(defn can-move-to?
  "In universe u, is the position [x y] empty?"
  [u [x y]]
  (if-let [c (get-in (u :maze) [y x])]
    (and (= c \.) (not (creature-at? u [x y])))))

(defn neighbors [u [x y]]
  (let [points (map (partial mapv + [x y]) '([0 1] [0 -1] [1 0] [-1 0]))]
    (zipmap (filter (partial can-move-to? u) points) (repeat 1))))

