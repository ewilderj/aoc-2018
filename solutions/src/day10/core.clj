(ns day10.core
  (:require [aoc.core :as aoc] [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [l]
  (->> l
       (re-seq #"\-?\d+")
       (map #(Integer. %))
       (partition 2)))

(def inp (map parse (aoc/puzzle-lines "day10")))

(defn bounding [l]
  (let [xs (map first l) ys (map second l)]
    [(apply min xs) (apply min ys) (apply max xs) (apply max ys)]))

(defn field-size [l]
  (let [[x1 y1 x2 y2] (bounding l)]
    (* (Math/abs (- x1 x2)) (Math/abs (- y1 y2)))))

(defn visualize [l]
  (let [[x1 y1 x2 y2] (bounding l)]
    (str/join "\n"
      (for [y (range y1 (inc y2))]
        (apply str (for [x (range x1 (inc x2))]
                     (if (some #{(list x y)} l) \# \.)))))))

(defn update-pos [p v]
  (map #(apply mapv + %) (partition 2 (interleave p v))))

(defn play [i]
  (loop [pos (map first i) vel (map second i)
         size (field-size pos) iter 0]
    (let [npos (update-pos pos vel) nsize (field-size npos)]
      (if (> nsize size) ;; we're done when the field stops contracting
        (do (println "in" iter "seconds..." ) (print (visualize pos)))
        (recur npos vel nsize (inc iter))))))

;; both parts can be solved by running (play inp)
