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
  [u p]
  (contains? #{\. \|} (maze-cell u p)))

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
