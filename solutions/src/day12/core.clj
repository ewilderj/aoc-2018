(ns day12.core
  (:require [aoc.core :as aoc] [clojure.string :as str]
            [clojure.set :as set]))

(def inp (aoc/puzzle-lines "day12"))
(def init-state (apply str (drop 15 (first inp))))
(def rules (into {} (map #(str/split % #" => ") (drop 2 inp))))
(def universe {:l 0 :d init-state})

(defn pad-l [u]
  (loop [l (u :l) d (u :d)]
    (if (str/starts-with? d ".....") {:l l :d d}
      (recur (dec l) (str/join ["." d])))))

(defn pad-r [u]
  (loop [d (u :d)]
    (if (str/ends-with? d ".....") {:l (u :l) :d d}
        (recur (str/join [d "."])))))

; ensure we expand to empty pots in case we need them
(defn pad [u] (pad-r (pad-l u)))

(defn play-turn [d]
  (loop [s (vec d) r []]
    (if (< (count s) 5)
      (apply str r)
      (let [p (apply str (take 5 s))
            q (get rules p ".")]
        (recur (rest s) (conj r q))))))

(defn score [uni]
  (loop [i (uni :l) d (vec (uni :d)) r 0]
    (if (empty? d) r
        (if (= (first d) \#)
          (recur (inc i) (rest d) (+ r i))
          (recur (inc i) (rest d) r)))))

;; part1: (play universe)
(defn play [uni]
  (loop [i 0 u (pad uni)]
    (if (= i 20)
      (score u)
      (let [n (play-turn (u :d))]
        (recur (inc i) (pad {:l (+ (u :l) 2) :d n}))))))

(def max-iter 50000000000)

;; strategy: after a certain number of iterations we see
;; that we get a pattern marching one pot right each turn;
;; the first time we see that, figure out and extrapolate
;; to max-iter turns - run with (play2 universe)
(defn play2 [uni]
  (loop [i 0 u (pad uni) seen {}]
    (if (= i max-iter)
      (score u)
      (let [n (play-turn (u :d))
            nu (pad {:l (+ (u :l) 2) :d n})]
        (if (seen (nu :d))
          (recur max-iter
                 (assoc nu :l (+ (- max-iter i 1) (nu :l)))
                 seen)
          (recur (inc i) nu (assoc seen (nu :d) i)))))))
