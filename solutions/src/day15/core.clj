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

(defn reachable
  "In universe u, all the squares reachable in one step from p"
  [u p]
  (let [points (map (partial mapv + p) '([0 1] [0 -1] [1 0] [-1 0]))]
    (filter (partial can-move-to? u) points)))

(defn reachable-scores
  "In universe u, all the squares reachable in one step from p, mapped to their cost"
  [u p]
  (zipmap (reachable u p) (repeat 1)))

(defn sort-creatures
  "Sort the creatures in universe u so they are in 'read-order"
  [u]
  (assoc u :creatures
         (->> (u :creatures)
              (sort-by ffirst)
              (sort-by (comp second first))
              vec)))

(defn viable-targets
  "In universe u, which targets does the creature at point p have?"
  [u p]
  (let [cm (into {} (u :creatures))
        k (if (= (cm p) \G) \E \G)]         ; figure out who's an enemy
    (assert (contains? cm p))               ; error if this isn't a critter
    (filter #(= (cm %) k) (keys cm))))      ; collect coords of enemies

(defn adjacent-targets
  "In universe u, all the squares adjacent to an enemy of the creature at point p"
  [u p]
  (->> (viable-targets u p)                 ; locate enemies
       (map (partial reachable u))          ; find open squares next to them
       (apply concat)))

(defn decide-destination
  "In universe u, for creature at p, locate the closest possible square
  adjacent to an enemy."
  [u p]
  (let [distances (dijkstra p (partial reachable-scores u))
        dests (set (adjacent-targets u p))
        feasible (into (priority-map)
                       (filter (fn [[k v]] (contains? dests k)) distances))]
    (if (empty? feasible) nil
        (let [d (apply min (vals feasible)) ; find the distance of the nearest
              t (->> feasible
                     (filter (fn [[k v]] (= d v)))
                     (keys) (sort))]        ; find all that are at that distance
          (first t)))))                     ; and take the first in sort order

(defn delta-to-dest
  "Given a creature at x y and destination p q, generate read-order [dx dy]"
  [[x y] [p q]]
  (cond
    (not= x p) [(Integer/signum (- p x)) 0]
    (not= y q) [0 (Integer/signum (- q y))]
    :else [0 0]))
