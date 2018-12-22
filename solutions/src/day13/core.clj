(ns day13.core
  (:require [aoc.core :as aoc] [clojure.string :as str]
            [clojure.set :as set]))

(def inp (aoc/puzzle-lines "day13-example"))

;; turn cart into the direction line it was on
(defn rem-cart [c] (if (#{\v \^} c) \| \-))
(defn a-cart? [c] (#{\v \> \< \^} c))
(defn parse-line [l y]
  (loop [d (vec l) carts [] r [] x 0]
    (if (empty? d)
      [r carts]
      (if (a-cart? (first d))
        (recur (rest d) (conj carts [[x y] (first d) :l])
               (conj r (rem-cart (first d))) (inc x))
        (recur (rest d) carts (conj r (first d)) (inc x))
      )
      )))

;; map for turning direction.
;; e.g.i'm going > when I hit a \
;; (get-in steer [\\ \>]) ==> \v 
(def steer {\/ { \^ \>, \< \v, \> \^, \v \< }
            \\ { \^ \<, \< \^, \> \v, \v \> }})

;; handle rotation through these maps
(def r-left  {\^ \<, \< \v, \v \>, \> \^})
(def r-right {\^ \>, \> \v, \v \<, \< \^})

;; given a direction d and heading c, return next c
(defn turn [d c]
  (if (= d :s) c
      (if (= d :l) (r-left c)
          (r-right c))))

;; next rotation direction based on current
(def next-dir {:l :s, :s :r, :r :l})

(defn parse-inp [i]
  (loop [i i carts [] r [] y 0]
    (if (empty? i)
      [r carts]
      (let [[l c] (parse-line (first i) y)]
        (recur (rest i) (concat carts c) (conj r l) (inc y))
      ))))

;; carts is a list of 3-tuples
;; [[x y] <current-direction> <dir-at-next-intersection>]
;; dir at next intersection is :l :r or :s
