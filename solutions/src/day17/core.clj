(ns day17.core
  (:require [aoc.core :as aoc] [clojure.string :as str]
            [clojure.set :as set] [clojure.spec.alpha :as s]
            [clojure.data.priority-map :refer [priority-map]]))

(def inp (aoc/puzzle-lines "day17"))
(def exinp (aoc/puzzle-lines "day17-ex"))

(defn draw-cell
  ([m c p] (draw-cell m c p false))
  ([m c p check]
   (if (and check (contains? #{\~ \#} (get m (vec p)))) ;; debugging measure in case of overdrawing
     (assert false "oh noooo")
     (assoc m (vec p) c))))

(defn draw-vert
  [u c x y1 y2]
  (let [cc (partition 2 (interleave
                         (partition 2 (interleave (repeat x)
                                                  (range y1 (inc y2))))
                         (repeat c)))]
    (reduce (fn [r [p c]] (draw-cell r c p)) u cc)))

(defn draw-horiz
  ([u c y x1 x2] (draw-horiz u c y x1 x2 false))
  ([u c y x1 x2 check]
  (let [cc (partition 2 (interleave
                         (partition 2 (interleave (range x1 (inc x2))
                                                  (repeat y)))
                         (repeat c)))]
    (reduce (fn [r [p c]] (draw-cell r c p check)) u cc))))

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
          (apply min (map (comp second first) maze))
          (apply max (map (comp second first) maze))))

(defn make-universe
  [src]
  (let [m (make-maze src)
        [_ _ y1 y2] (bounds m)]
    {:maze m :min-y y1 :max-y y2}))

(defn render-universe
  [u]
  (let [maze (u :maze)
        [x1 x2 _ _] (bounds maze)                   ;; any x is good, so don't use original bounds
        [y1 y2] (map u [:min-y :max-y])]            ;; but ys are limited to the scan boundary
    (str/join "\n"
              (for [y (range y1 (inc y2))]
                (apply str (for [x (range x1 (inc x2))]
                             (maze-cell maze [x y])))))))

(defn pud [u] (println (render-universe u)))

(defn walk-down
  "Walks down from point p until it hits an obstacle. Returns new
  universe and end pt if there's an obstacle, nil if ran out of bounds."
  [u p]
  (let [max-y (u :max-y) x (first p)]
    (loop [m (u :maze) y (second p)]
      (cond
        (> y max-y) [(assoc u :maze m) nil]                       ;; ran out of map
        (traversable? m [x y]) (recur (assoc m [x y] \|) (inc y)) ;; ok? keep going down
        :else [(assoc u :maze m) [x (dec y)]]                     ;; hit obstacle, report last good coords
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
    (if (false? (or left-gap right-gap))                       ;; obstacles both sides
      [(assoc u :maze (draw-horiz m \~ ly lx rx true)) []]     ;; draw water, checking not overdrawing 
                                                               ;; otherwise, there's a gap one or both sides
      (let [u' (assoc u :maze (draw-horiz m \| ly lx rx true)) ;; draw path, checking not overdrawing
            np (vec (filter identity [(if left-gap [lx ly])
                                 (if right-gap [rx ry])]))]
        [u' np]))))

(defn walk
  ([u] (walk u [500 0])) ;; start at spring
  ([u p]
   ;; (pud u)
   (loop [u u]
     (let [blacklist (get u :blacklist #{})]
       (let [[u' cpoint] (walk-down u p)]
         (cond
           (nil? cpoint) (assoc u' :blacklist (conj blacklist p))    ;; hit the end of the map, return new univers
           (contains? blacklist cpoint) (assoc u' :blacklist (conj blacklist p)) ;; got to a blacklisted point, ripple up
           (< (second cpoint) (second p)) (assoc u' :restart true)   ;; flooded our start point, retry from parent
           :else ;; explore to the left and right
           (let [[u'' nsts] (explore-horiz u' cpoint)]               ;; fill water, find any nexts (gaps)
             (if (empty? nsts)
               (recur u'')                                           ;; no gaps, start again from the previous point
               (let [n2 (remove #(contains? blacklist %) nsts)]
                 (if (empty? n2)            ;; all my options from here are blacklisted, so blacklist my start point
                   (assoc u'' :blacklist (conj blacklist cpoint)) 
                   (let [u3 (reduce (fn [r n] (walk r n)) u'' n2)]   ;; else walk my nexts
                     (if (u3 :restart)                               ;; retry (previous start point got flooded)
                       (recur (dissoc u3 :restart))
                       u3))))))))))))                               ;; if not restart, just return

(defn part1 [i]
  (count (filter #{\~ \|} (render-universe (walk (make-universe i))))))

(defn part2 [i]
  (count (filter #{\~} (render-universe (walk (make-universe i))))))
