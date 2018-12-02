(ns aoc.core
  (:require [clojure.string :as str]))

(defn puzzle-data [s]
  (slurp (str "/home/edd/work/github/aoc-2018/solutions/resources/" s ".txt")))

(defn puzzle-lines [s]
  (str/split-lines (puzzle-data s)))

(defn puzzle-ints [s]
  (map #(Integer. %) (puzzle-lines s)))

