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
  "Read a line of maze, and remove creatures into a separate map"
  [y l]
  (loop [l l creatures {} r [] x 0]
    (if (empty? l) [r creatures]
        (let [c (first l)]
          (if (#{\G \E} c) 
            (recur (rest l) (conj creatures [[x y] [c 200 3]]) (conj r \.) (inc x))
            (recur (rest l) creatures (conj r c) (inc x)))))))

(defn universe
  "A map used for game state"
  [maze creatures] {:maze maze :creatures creatures})

(defn read-order-comparator
  "Comparator for maintaining the read order"
  [[a b] [c d]]
  (if (= b d)
    (< a c)
    (< b d)))

(defn parse-input
  "Process all puzzle lines from a vector of lines. Store creatures
  in a sorted map, which by default will preserve the read-order the
  problem calls for."
  [i]
  (let [s (map-indexed parse-line i)]
    (universe (vec (map first s))
              (into (sorted-map-by read-order-comparator)
                    (map second s)))))

(defn creature-at?
  "In universe u, is there a creature at [x y]?"
  [u [x y]]
  (contains? (u :creatures) [x y]))

(defn can-move-to?
  "In universe u, is the position [x y] empty?"
  [u [x y]]
  (if-let [c (get-in (u :maze) [y x])]
    (and (= c \.) (not (creature-at? u [x y])))))

(def adjacencies
  "Deltas to adjacent squares, in read order"
  '([0 -1] [-1 0] [1 0] [0 1]))

(defn adjacent-points
  "All move-legal adjacent points to input point p"
  [p] (map (partial mapv + p) adjacencies))

(defn reachable
  "In universe u, all the squares reachable in one step from p"
  [u p]
  (filter (partial can-move-to? u) (adjacent-points p)))

(defn reachable-scores
  "In universe u, all the squares reachable in one step from p, mapped to their cost"
  [u p]
  (zipmap (reachable u p) (repeat 1)))

(def enemy {\G \E, \E \G})

(defn viable-targets
  "In universe u, which targets does the creature at point p have?"
  [u p]
  (assert (contains? (u :creatures) p))      ; error if this isn't a critter
  (let [c (u :creatures)
        k (enemy (first (c p)))]             ; figure out who's an enemy
    (filter #(= (first (c %)) k) (keys c)))) ; collect coords of enemies

(defn adjacent-targets
  "In universe u, all the squares adjacent to an enemy of the creature at point p"
  [u p]
  (->> (viable-targets u p)                  ; locate enemies
       (map (partial reachable u))           ; find open squares next to them
       (apply concat)))

(defn sort-read-order
  "Sort a coordinate list to read order"
  [l]
  (->> l
       (sort-by first)
       (sort-by second)))

(defn decide-destination
  "In universe u, for creature at p, locate the closest possible square
  adjacent to an enemy."
  [u p]
  (let [distances (dijkstra p (partial reachable-scores u))
        dests (set (adjacent-targets u p))
        feasible (into (priority-map)
                       (filter (fn [[k v]] (contains? dests k)) distances))]
    (if (empty? feasible) nil
        (let [d (apply min (vals feasible))     ; find the distance of the nearest
              t (->> feasible
                     (filter (fn [[k v]] (= d v)))
                     (keys) (sort-read-order))] ; find all that are at that distance
          (first t)))))                         ; and take the first in sort order

(defn delta-to-dest
  "Given a creature at x y and destination p q, generate read-order [dx dy]"
  [[x y] [p q]]
  ;; read order is up, left, right, down
  (cond
    (< q y) [0 -1]
    (not= x p) [(Integer/signum (- p x)) 0]
    (> q y) [0 1]
    :else [0 0]))

(defn hitpoint-table
  "Print out each creature and their hit points"
  [u]
  (str/join "\n" (map str (u :creatures))))

(defn render-universe
  "Print a universe, for debug purposes"
  [u]
  (str/join "\n" (for [y (range (count (u :maze)))]
    (apply str (for [x (range (count (first (u :maze))))]
      (if-let [c (get-in u [:creatures [x y] 0])]
        c (get-in u [:maze y x]))
      )))))

(defn pud
  "Debug function to inspect state"
  [u]
  (println (render-universe u) "\n")
  (println (hitpoint-table u)))

(defn what-to-attack
  "Given a universe u and a creature a point p, which to attack"
  [u p]
  (let [attacker (get-in u [:creatures p])
        ilk (enemy (first attacker))
        splash-zone (set (adjacent-points p))
        candidates (filter                     ; find critters in the splash zone
                    (fn [[k v]]
                      (and (contains? splash-zone k)
                           (= ilk (first v)))) ; that are of enemy ilk!
                    (u :creatures))
        ;; lowest hit points first, then in read order
        c-by-hp (sort-by (comp second second)
                         (sort-by first candidates))]
    (ffirst c-by-hp)))

(defn attack
  "In universe u, aggressor at point a attacks the victim at point v"
  [u a v]
  (let [attacker (get-in u [:creatures a])
        victim (get-in u [:creatures v])
        attack-points (nth attacker 2)
        hit-points (second victim)
        nhp (- hit-points attack-points)
        victim' (assoc-in victim [1] nhp)]
    (println "Attack! By" a v ":" attacker "on" victim "=>" victim')
    (if (<= nhp 0)
      (assoc u :creatures (dissoc (u :creatures) v))
      (assoc-in u [:creatures v] victim'))
  ))

(defn move-coords
  "In universe u, return the coords of the next step a unit at p takes"
  [u p]
  (if-let [d (decide-destination u p)]
    (mapv + p (delta-to-dest p d))
    (assert p "nowhere to go")
    ))

(defn creature-move
  "In universe u, play the turn for the creature at p"
  [u p]
  (if-let [enemy (what-to-attack u p)] 
    (attack u p enemy)                         ; attack if we're near an enemy
    (let [p' (move-coords u p)
          nc (assoc (dissoc (u :creatures) p) p' (get-in u [:creatures p]))
          u' (assoc u :creatures nc)
          enemy'  (what-to-attack u' p')]
      (if (nil? enemy')
        u'
        (attack u' p' enemy')
      )
    )
  ))


