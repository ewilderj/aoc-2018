(ns day15.core
  (:require [aoc.core :as aoc] [clojure.string :as str]
            [clojure.set :as set] [clojure.spec.alpha :as s]
            [clojure.data.priority-map :refer [priority-map]]))

(def inp (aoc/puzzle-lines "day15"))

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
  [u [x y]] (contains? (u :creatures) [x y]))

(defn can-move-to?
  "In universe u, is the position [x y] empty?"
  [u [x y]]
  (if-let [c (get-in (u :maze) [y x])]
    (and (= c \.) (not (creature-at? u [x y])))))

(def adjacencies
  "Deltas to adjacent squares, in read order (up, left, right, down)"
  '([0 -1] [-1 0] [1 0] [0 1]))

(defn adjacent-points
  "All move-legal adjacent points to input point p"
  [p] (map (partial mapv + p) adjacencies))

(defn reachable
  "In universe u, all the squares reachable in one step from p"
  [u p] (filter (partial can-move-to? u) (adjacent-points p)))

(defn reachable-scores
  "In universe u, all the squares reachable in one step from p, mapped to their cost"
  [u p] (zipmap (reachable u p) (repeat 1)))

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

(def sort-read-order (comp (partial sort-by second) (partial sort-by first)))

(defn move-by-delta
  "Add a delta d to coordinate p"
  [p d] (mapv + p d))

(defn decide-destination
  "In universe u, for creature at p, locate the closest possible square
  adjacent to an enemy."
  [u p]
  (let [distances (dijkstra p (partial reachable-scores u))
        dests (set (adjacent-targets u p))
        feasible (into (priority-map)
                       (filter (fn [[k v]] (contains? dests k)) distances))]

    (if (seq feasible)
      (let [d (apply min (vals feasible))     ; find the distance of the nearest
            target (first (sort-read-order
                           (map first (get (group-by val feasible) d))))

            ;; now we have the target, compute how far every square is from it
            target-distances (dijkstra target (partial reachable-scores u))
            ;; consider every step we can take, in reading order
            opts (filter (partial can-move-to? u)
                         (map (partial move-by-delta p) adjacencies))
            ;; further, consider only those reachable
            opts' (filter #(contains? target-distances %) opts)]

        (if (seq opts')                        ; if we still have options...
          (let [dmap (zipmap opts' (map target-distances opts')) ; dist of each from target
                s (apply min (vals dmap))] ; shortest distance

            ;; find first square with the shortest distance, in read-order
            (first (sort-read-order (map first (get (group-by val dmap) s))))))))))

(defn hitpoint-table
  "Describe each creature and their hit points"
  [u]
  (str/join "\n" (map str (u :creatures))))

(defn render-universe
  "Print a universe, for debug purposes"
  [u]
  (str/join "\n" (for [y (range (count (u :maze)))]
    (apply str (for [x (range (count (first (u :maze))))]
      (if-let [c (get-in u [:creatures [x y] 0])]
        c (get-in u [:maze y x])))))))

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
                    (u :creatures))]

    (if (seq candidates)                       ; if we found any nearby enemies
      (let [hit-points (comp second second)    ; helper for readability
            lowest-hp (apply min (map hit-points candidates))]

            (->> candidates
                 (filter #(= lowest-hp (hit-points %)))
                 (map first)
                 (sort-read-order)
                 (first))))))


(defn attack
  "In universe u, aggressor at point a attacks the victim at point v"
  [u a v]
  (let [attacker (get-in u [:creatures a])
        victim (get-in u [:creatures v])
        attack-points (nth attacker 2)
        hit-points (second victim)
        nhp (- hit-points attack-points)
        victim' (assoc-in victim [1] nhp)]

    (if (<= nhp 0)
      (assoc u :creatures (dissoc (u :creatures) v))  ; creature is dead, remove
      (assoc-in u [:creatures v] victim'))            ; else update hit points
  ))

(defn creature-move
  "In universe u, play the turn for the creature at p"
  [u p]
  (if-let [e (what-to-attack u p)]
    (attack u p e)                             ; attack if we're near an enemy
    (if-let [p' (decide-destination u p)]      ; if there's a move we can make
      (let [nc (assoc (dissoc (u :creatures) p) p' (get-in u [:creatures p]))
            u' (assoc u :creatures nc)         ; update creature position
            e' (what-to-attack u' p')]         ; is there now an enemy near?
        (if e' (attack u' p' e') u'))          ; yes, attack! no, send universe
      u)))                                     ; no move available, no change

(defn winner?
  "True if one of the teams is extinct in universe u"
  [u]
  (< (count (frequencies (map first (vals (u :creatures))))) 2))

(defn one-turn
  "Play a turn in universe v"
  [v]
  (loop [creatures (keys (v :creatures))      ; will be sorted in read order
         u v]                                 ; universe, will change after move
      (if (empty? creatures)                  ; have we moved all creatures?
        u                                     ; yes, return the universe
        (if (winner? u)                       ; if no targets left, we're done
          (assoc u :won true)                 ; send back universe in won state
          (if (contains? (u :creatures) (first creatures)) ; check not dead yet
            (recur (rest creatures) (creature-move u (first creatures)))
            (recur (rest creatures) u))))))


(defn play
  "Play the game with input i, applying f to universe before running"
  [universe]
  (loop [u universe n 0]
    ;; (print n ".") (flush)
    (let [u' (one-turn u)]
      (if (u' :won)
        (let [v (reduce + (map second (vals (u' :creatures))))]
          [(* n v) u'])
        (recur u' (inc n))))))

(defn part1 [] (first (play (parse-input inp)))) ; (part1) => 183300

;; current bug is around turn 23
;; delta to dest doesn't check if you're about to move to
;; an empty square or not

(defn set-elf-ap
  [u ap]
  (assoc u :creatures
         (into (sorted-map-by read-order-comparator)
               (map (fn [[k v]] {k (if (= \E (first v)) [\E 200 ap] v)})
                    (u :creatures)))))

(defn elf-count
  [u] ((frequencies (map (comp first second) (u :creatures))) \E))

(defn part2 [] ; (part2) => 40625
  (let [u (parse-input inp)
        num-elves (elf-count u)]
    (println "Target number of elves is" num-elves)
    (loop [eap 33] ; from previous runs I learned a good start
      (println "Playing with AP" eap)
      (let [[outcome u'] (play (set-elf-ap u eap))
            ec (elf-count u')]
        (println ec "elves survived, outcome" outcome)
        (if (or (nil? ec) (< ec num-elves))
          (recur (inc eap))
          outcome)))))

