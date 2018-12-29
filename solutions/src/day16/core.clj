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

(def p2data
  "Parse the instructions for part 2"
  (map to-ints (subvec inp (+ 2 (* (count p1data) 4))))
  )

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
  (map first
       (filter (fn [[k v]] (= v after))
               (zipmap all-ops (map #(% before a b c) all-ops)))))

(defn part1 [] ; => 570
  (count
   (filter #(>= % 3)
           (map count
               (map (partial apply which-ops) p1data)))))

(defn find-key-with-one-opcode
  [m]
  (filter (fn [[k v]] (= (count v) 1)) m))

(defn remove-opcode
  [m op]
  (reduce (fn [r [k v]] (assoc r k (set (remove #{op} v)))) {} m))

(defn compute-instruction-set
  []
  (let [ops-and-matches
        ;; for each example, compute candidate instrs for each op code
        (partition 2 (interleave (map (comp first second) p1data)
                                 (map (partial apply which-ops) p1data)))
        ;; summarize these findings into a set for each opcode
        summary (reduce (fn [r el]
                          (assoc r (first el) (set/union (get r (first el) #{})
                                                         (set (second el)))))
                        {} ops-and-matches)
        ]
    ;; when an opcode has only one candidate, we assign it, remove, and repeat
    (loop [s summary r {}]
      (if (empty? s) r
          (if-let [[[k v]] (find-key-with-one-opcode s)]
            (recur (remove-opcode (dissoc s k) (first v))
                   (assoc r k (first v))))))))

(defn part2 ; => 503
  []
  (let [iss (compute-instruction-set)]
    (loop [r [0 0 0 0] i p2data]
      (if (empty? i) (first r)
          (let [[op a b c] (first i)
                ifn (iss op)]
            (recur (ifn r a b c) (rest i)))))))
