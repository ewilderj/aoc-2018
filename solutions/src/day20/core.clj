(ns day20.core
  (:require [aoc.core :as aoc] [clojure.string :as str]
            [clojure.set :as set]))

(def inp
  (let [s (aoc/puzzle-data "day20")
        c (count s)]
    (subs s 1 (- c 2))))

(def ex "WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))")

(def delta {\N [0 -1] \S [0 1] \E [1 0] \W [-1 0]})
(defn move [p d] (mapv + p (delta d)))

(defn visit-room [rooms p d]
  (assoc rooms p (min d (get rooms p Integer/MAX_VALUE))))

(defn play
  "Run the simulation - s is the regexp string
  rooms is the original state of rooms and the distance to them
  h is a history of positions when a choice is encountered
  p is the starting position"
  [s]
  (loop [s s rooms {[0 0] 0} h [] p [0 0]]
    (if (empty? s) rooms
        (let [i (first s)
              s' (rest s)]
          (cond
            (#{\N \E \W \S} i) ;; direction
            (let [p' (move p i)
                  rooms' (visit-room rooms p' (inc (rooms p)))]
              (recur s' rooms' h p'))

            (= i \() ;; open choice
            (recur s' rooms (conj h p) p)

            (= i \|) ;; choice delimiter
            (recur s' rooms h (peek h))

            (= i \))
            (recur s' rooms (pop h) (peek h)))))))

(defn part1 [] (->> inp (play) (vals) (apply max)))

(defn part2 [] (->> inp
                    (play)
                    (vals)
                    (filter #(>= % 1000))
                    (count)))


;; note for the future: I struggled hard at this one
;; (probably didn't understand the question properly)
;; and had to take some clues from a Python answer.
