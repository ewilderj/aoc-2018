(ns day15.core
  (:require [aoc.core :as aoc] [clojure.string :as str]
            [clojure.set :as set] [clojure.spec.alpha :as s]))


(def inp (aoc/puzzle-lines "day15-ex1"))

(defn parse-line [y l]
  (loop [l l creatures [] r [] x 0]
    (if (empty? l) [r creatures]
        (let [c (first l)]
          (if (#{\G \E} c) 
            (recur (rest l) (conj creatures [[x y] c]) (conj r \.) (inc x))
            (recur (rest l) creatures (conj r c) (inc x)))))))

(defn parse-input [i]
  (let [s (map-indexed parse-line i)
        maze (map first s)
        creatures (apply concat (map second s))]
    [maze creatures]))
