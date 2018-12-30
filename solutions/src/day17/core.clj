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
    (reduce (fn [r [p c]] (assoc r (vec p) c)) u cc)
  ))

(defn draw-horiz
  [u c y x1 x2]
  (let [cc (partition 2 (interleave
                         (partition 2 (interleave (range x1 (inc x2))
                                                  (repeat y)))
                         (repeat c)))]
    (reduce (fn [r [p c]] (assoc r (vec p) c)) u cc)
    ))

(defn make-universe
  "Turn a vector of input lines into the universe"
  [src]

  (loop [i src u (sorted-map)]
    (if (empty? i) u
        (let [[j a b] (map #(Integer. %) (re-seq #"\d+" (first i)))]
          (if (= (ffirst i) \x)
            (recur (rest i) (draw-vert u \# j a b))
            (recur (rest i) (draw-horiz u \# j a b))
            )
          )
      )
  ))
