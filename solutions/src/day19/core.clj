(ns day19.core
  (:require [aoc.core :as aoc] [clojure.string :as str]
            [clojure.set :as set] [clojure.spec.alpha :as s]))

(def inp (aoc/puzzle-lines "day19"))
(def inp (aoc/puzzle-lines "day19-ex"))

(defmacro defop
  "Defines an op code. f should expect args a, b, c"
  [n f]
  `(def ~n
     (fn ~@'([r ^long a ^long b ^long c])
       (assoc ~'r ~'c ~f))))

(defop addr (+ (r a) (r b)))
(defop addi (+ (r a) b))
(defop mulr (* (r a) (r b)))
(defop muli (* (r a) b))
(defop banr (bit-and (r a) (r b)))
(defop bani (bit-and (r a) b))
(defop borr (bit-or (r a) (r b)))
(defop bori (bit-or (r a) b))
(defop setr (r a))
(defop seti a)
(defop gtir (if (> a (r b)) 1 0))
(defop gtri (if (> (r a) b) 1 0))
(defop gtrr (if (> (r a) (r b)) 1 0))
(defop eqir (if (= a (r b)) 1 0))
(defop eqri (if (= (r a) b) 1 0))
(defop eqrr (if (= (r a) (r b)) 1 0))

(defn make-computer
  [i]
  )
