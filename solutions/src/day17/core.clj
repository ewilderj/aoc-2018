(ns day17.core
  (:require [aoc.core :as aoc] [clojure.string :as str]
            [clojure.set :as set] [clojure.spec.alpha :as s]
            [clojure.data.priority-map :refer [priority-map]]))

(def inp (aoc/puzzle-lines "day17"))
(def exinp (aoc/puzzle-lines "day17-ex"))

(defn draw-vert
  [u c x y1 y2]
  (let [cc (partition 2 (interleave
                         (partition 2 (interleave (repeat x)
                                                  (range y1 (inc y2))))
                         (repeat c)))]
    (reduce (fn [r [p c]] (assoc r (vec p) c)) u cc)))

(defn draw-horiz
  [u c y x1 x2]
  (let [cc (partition 2 (interleave
                         (partition 2 (interleave (range x1 (inc x2))
                                                  (repeat y)))
                         (repeat c)))]
    (reduce (fn [r [p c]] (assoc r (vec p) c)) u cc)))

(defn make-maze
  "Turn a vector of input lines into the maze"
  [src]

  (loop [i src u (sorted-map)]
    (if (empty? i) u
        (let [[j a b] (map #(Integer. %) (re-seq #"\d+" (first i)))]
          (if (= (ffirst i) \x)
            (recur (rest i) (draw-vert u \# j a b))
            (recur (rest i) (draw-horiz u \# j a b)))))))

(defn maze-cell
  [maze point]
  (get maze point \.))

(defn traversable?
  [m p]
  (contains? #{\. \|} (maze-cell m p)))

(defn bounds
  "Returns min-x, max-x and max-y of the maze. min-y is always 0"
  [maze]
  (vector (apply min (map ffirst maze))
          (apply max (map ffirst maze))
          (apply max (map (comp second first) maze))))

(defn make-universe
  [src]
  (let [m (make-maze src)
        [x1 x2 y] (bounds m)]
    {:maze (assoc m [500 0] \+)
     :min-x x1 :max-x x2 :max-y y}))

(defn render-universe
  [u]
  (let [maze (u :maze)
        [x1 x2 yy] (map u [:min-x :max-x :max-y])]
    (str/join "\n"
              (for [y (range 0 (inc yy))]
                (apply str (for [x (range x1 (inc x2))]
                             (maze-cell maze [x y])))))))

(defn pud [u] (print (render-universe u)))

(defn walk-down
  "Walks down from point p until it hits an obstacle. Returns new
  universe and true if there's an obstacle, false if ran out of bounds."
  [u p]
  (let [max-y (u :max-y) x (first p)]
    (loop [m (u :maze) y (second p)]
      (cond
        (>= y max-y) [(assoc u :maze m) false] ;; ran out of space
        (traversable? m [x y]) (recur (assoc m [x y] \|) (inc y))
        :else [(assoc u :maze m) true]              ;; hit obstacle 
        ))))

(defn walk-horiz
  "Walks across from point p in direction dx until it hits a gap underneath
  or it hits an obstacle. Returns terminating coord, and true if gap."
  [u p dx]
  (let [m (u :maze) y (second p)]
    (loop [x (first p)]
      (cond
        (not (traversable? m [x y])) ;; when there's an obstacle
        [[(- x dx) y] false]         ;; previous point is the edge
        (traversable? m [x (inc y)]) ;; when there's a gap
        [[x y] true]
        :else                        ;; let's keep on walking
        (recur (+ x dx))))))

(defn explore-horiz
  "Explores horizontally from starting point and returns any
  new gaps it finds for further vertical exploration."
  [u p]
  (let [[[lx ly] left-gap] (walk-horiz u p -1)
        [[rx ry] right-gap] (walk-horiz u p 1)
        m (u :maze)]
    (if (false? (or left-gap right-gap))              ;; obstacles both sides
      [(assoc u :maze (draw-horiz m \~ ly lx rx)) []] ;; draw water
      ;; otherwise, there's a gap one or both sides
      (let [u' (assoc u :maze (draw-horiz m \| ly lx rx))
            np (filter identity [(if left-gap [lx ly])
                                 (if right-gap [rx ry])])]
        [u' np]))))
