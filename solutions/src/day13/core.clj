(ns day13.core
  (:require [aoc.core :as aoc] [clojure.string :as str]
            [clojure.set :as set]))

(def inp (aoc/puzzle-lines "day13-example"))
(def inp (aoc/puzzle-lines "day13"))

;; turn cart into the direction line it was on
(defn rem-cart [c] (if (#{\v \^} c) \| \-))
(defn a-cart? [c] (#{\v \> \< \^} c))

;; record the roads on line y, swapping out carts when we find em
;; a cart is a 3-tuple
;; [[x y] <current-direction> <dir-at-next-intersection>]
;; dir at next intersection defaults to :l
(defn parse-line [l y]
  (loop [d (vec l) carts [] r [] x 0]
    (if (empty? d)
      [r carts]
      (if (a-cart? (first d))
        (recur (rest d) (conj carts [[x y] (first d) :l])
               (conj r (rem-cart (first d))) (inc x))
        (recur (rest d) carts (conj r (first d)) (inc x))))))

;; parse the map line by line, return roads and carts
(defn parse-inp [i]
  (loop [i i carts [] r [] y 0]
    (if (empty? i)
      [r carts]
      (let [[l c] (parse-line (first i) y)]
        (recur (rest i) (concat carts c) (conj r l) (inc y))))))


;; map for turning direction, e.g. (get-in steer [\\ \>]) ==> \v 
(def steer {\/ { \^ \>, \< \v, \> \^, \v \< }
            \\ { \^ \<, \< \^, \> \v, \v \> }})

;; handle rotation through these maps
(def r-left  {\^ \<, \< \v, \v \>, \> \^})
(def r-right {\^ \>, \> \v, \v \<, \< \^})

;; coord deltas per direction, add \C for crashed (goes nowhere)
(def dxy {\^ [0 -1], \< [-1 0], \v [0 1], \> [1 0], \C [0 0]})

;; given a direction d and heading c, return next c
(defn turn [d c]
  (if (= d :s) c
      (if (= d :l) (r-left c) (r-right c))))

;; next rotation direction based on current
(def next-dir {:l :s, :s :r, :r :l})

;; figure out what to do for next road piece
(defn turn-or-steer [npos [xy heading turndir]]
  (if (#{\\ \/} npos)
    [(get-in steer [npos heading]) turndir]
    (if (= \+ npos)
      [(turn turndir heading) (next-dir turndir)]
      (if (#{\- \|} npos)
        [heading turndir]
        (println "PROBLEM" npos)))))           ;; last only useful when debugging

(defn move-cart [roads cart]
  (if (= (second cart) \C) cart                ;; ignore crashed cart, it gets removed
      (let [[xy heading turndir] cart          ;; else compute next coords, heading, etc.
            [nx ny] (mapv + xy (dxy heading))
            npos (get-in roads [ny nx])
            [nh nt] (turn-or-steer npos cart)]
        [[nx ny] nh nt])))

;; crash at a non-unique set of coordinates
(defn crash-coordinates [carts]
  (map first (filter #(> (val %) 1) (frequencies (map first carts)))))

;; for every cart, move it, and stop if we find a crash
(defn move-carts [roads carts]
  (loop [i 0 carts carts crash []]
    (if (or (seq crash) (>= i (count carts)))
      [carts crash]
      (let [ncarts (assoc-in carts [i] (move-cart roads (get carts i)))]
        (recur (inc i) ncarts (concat crash (crash-coordinates ncarts)))))))

;; sort cart array to ensure evaluation order is correct (y then x)
(defn cart-sort [v]
  (->> v
       (sort-by ffirst)
       (sort-by (comp second first))
       (vec)))

;; keep ticking the clock til we find a crash
(defn play [i]
  (let [[roads carts] (parse-inp i)]
    (loop [carts (vec carts) crash []]
      (if (seq crash)
        (first crash)
        (let [[ncarts ncrash] (move-carts roads carts)]
          (recur (cart-sort ncarts) ncrash))))))

;; part1: (play inp) => [41 22]

;; part2 strategy - we flag crashed carts as we go through
;; and remove them after the end of each turn

(defn flag-crashed [xy carts]
  (vec (map #(if (= xy (first %)) [[-100 -100] \C :crashed] %) carts)))

(defn remove-crashed [carts]
  (vec (filter #(not (= \C (second %))) carts)))

(defn move-carts2 [roads carts]
  (loop [i 0 carts carts]
    (if (>= i (count carts))
      (remove-crashed carts)
      (let [ncarts (assoc-in carts [i] (move-cart roads (get carts i)))
            coords (crash-coordinates (remove-crashed ncarts))]
        (if (seq coords)
          (recur (inc i) (flag-crashed (first coords) ncarts))
          (recur (inc i) ncarts))))))

(defn play2 [i]
  (let [[roads carts] (parse-inp i)]
    (loop [carts (cart-sort (vec carts))]
      (if (= 1 (count carts))
        (ffirst carts)
        (recur (cart-sort (move-carts2 roads carts)))))))

;; part2: (play2 inp) => [84 90]
