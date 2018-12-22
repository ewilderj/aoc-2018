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

;; coord deltas per direction
(def dxy {\^ [0 -1], \< [-1 0], \v [0 1], \> [1 0]})

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
        (println "PROBLEM, next position is" npos)))))

(defn move-cart [roads cart]
  (let [[xy heading turndir] cart
        [nx ny] (mapv + xy (dxy heading))
        npos (get-in roads [ny nx])
        [nh nt] (turn-or-steer npos cart)]
    ;; (println "cart" cart "new pos" nx ny "where there is" npos "nh" nh "nt" nt)
    [[nx ny] nh nt]))

;; for every cart, move it, and stop if we find a crash
(defn move-carts [roads carts]
  (loop [i 0 carts carts crash []]
    (if (or (seq crash) (>= i (count carts)))
      [carts crash]
      (let [ncarts (assoc-in carts [i] (move-cart roads (get carts i)))
            cf (filter #(> (val %) 1) (frequencies (map first ncarts)))]
        (recur (inc i) ncarts (concat crash cf))))))

;; sort cart array to ensure evaluation order is correct (y then x)
(defn cart-sort [v]
  (->> v
       (sort-by ffirst)
       (sort-by (comp second first))
       (vec)))

;; keep ticking the clock til we find a crash
(defn play [i]
  (let [[roads carts] (parse-inp i)]
    (loop [tick 0 carts (vec carts) crash []]
      (if (or (seq crash) (> tick 2000))
        (ffirst crash)
        (let [[ncarts ncrash] (move-carts roads carts)]
          (recur (inc tick) (cart-sort ncarts) ncrash))))))

;; part1: (play inp) => [41 22]
 
(defn move-carts2 [roads carts]
  (loop [i 0 carts carts crash []]
    (if (or (seq crash) (>= i (count carts)))
      [carts crash]
      (let [ncarts (assoc-in carts [i] (move-cart roads (get carts i)))
            cf (filter #(> (val %) 1) (frequencies (map first ncarts)))]
        (recur (inc i) ncarts (concat crash cf))))))

(defn play2 [i]
  (let [[roads carts] (parse-inp i)]
    (loop [tick 0 carts (cart-sort (vec carts)) crash []]
      (println "tick" tick "carts" carts)
      (if (or (seq crash) (> tick 2000))
        crash
        (let [[ncarts ncrash] (move-carts roads carts)]
          (recur (inc tick) (cart-sort ncarts) ncrash)
          ))
      )))
