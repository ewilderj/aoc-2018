(ns day10.core
  (:require [aoc.core :as aoc] [clojure.string :as str]
            [clojure.set :as set]))


(defn parse [l]
  (->> l
       (re-seq #"\-?\d+")
       (map #(Integer. %))
       (partition 2)
       ))
(def inp (map parse (aoc/puzzle-lines "day10")))

(defn max-x [l]
  (map ffirst l)
  )
