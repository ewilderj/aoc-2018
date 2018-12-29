(ns day16.core
  (:require [aoc.core :as aoc] [clojure.string :as str]
            [clojure.set :as set] [clojure.spec.alpha :as s]
            [clojure.data.priority-map :refer [priority-map]]))

(def init-state
  "Should we need it for debugging, empty register array"
  [0 0 0 0])

(def inp (aoc/puzzle-lines "day16"))

(defn to-ints
  "Remove all cruft and return a vector of ints in the input"
  [d]
  (->> d
       (re-seq #"\d+")
       (map #(Integer. %))
       (vec)))

(def p1data
  "Parse the data we care about for part 1 of the problem"
  (->> inp
       (partition 4)
       (filter #(str/starts-with? (first %) "Before"))
       (map (partial take 3))
       (map (partial map to-ints))
       ))

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

(def all-ops [addr, addi, mulr, muli,
              banr, bani, borr, bori,
              setr, seti,
              gtir, gtri, gtrr,
              eqir, eqri, eqrr])

(defn which-ops
  "Return the ops that match 'after' after application"
  [before [opcode a b c] after]
  ;; (println before [opcode a b c] after)
  (map first
       (filter (fn [[k v]] (= v after))
               (zipmap all-ops (map #(% before a b c) all-ops)))))


(defn part1 []
  (count
   (filter #(>= % 3)
           (map count
               (map (partial apply which-ops) p1data)))))

(defn part2 []
   (filter #(= (count %) 1)
           (map (partial apply which-ops) p1data)))


