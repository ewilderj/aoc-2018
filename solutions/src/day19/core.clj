(ns day19.core
  (:require [aoc.core :as aoc] [clojure.string :as str]
            [clojure.set :as set] [clojure.spec.alpha :as s]))

(def inp (aoc/puzzle-lines "day19"))
;;(def inp (aoc/puzzle-lines "day19-ex"))

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

(defn parse-line [l]
  (let [b (str/split l #"\s+")]
    (concat [(resolve (symbol (first b)))] (map #(Integer. %) (rest b)))))

(defn make-computer
  [i]
  (let [ip (Integer. (re-find #"\d" (first inp)))
        prog (vec (map parse-line (rest inp)))]
    {:ip ip, :prog prog, :r [0 0 0 0 0 0]}))

(defn ip [c] (get-in c [:r (c :ip)]))
(defn inc-ip [c] (assoc-in c [:r (c :ip)] (inc (ip c))))
(defn dec-ip [c] (assoc-in c [:r (c :ip)] (dec (ip c))))
(defn execute [c i]
  (let [[f x y z] i]
    (assoc c :r (f (c :r) x y z))))

(defn run
  [i r0]
  (loop [c (assoc-in (make-computer i) [:r 0] r0)]
    (if-let [ops (get-in c [:prog (ip c)])] ;; if there's a next instruction
      (recur (inc-ip (execute c ops)))
      (dec-ip c))))

(defn part1 [i] (get-in (run i 0) [:r 0])) ;; => 1326
;; part2 would take forever =>
(def part2 (reduce + [1 3 41 109 123 327 787 2361 4469 13407 32267 85783 96801 257349 3517103 10551309]))

;; This code sums the factors of a number. When r0 = 0, it's 909.
;; When it's 1, it's 10551309

;; 0  addi 2 16 2     ; jump to 17 (pc = 16, then +1)
;; 1  seti 1 1 5      ; r5 = 1
;; 2  seti 1 1 3      ; r3 = 1
;; 3  mulr 5 3 4      ; r4 = r3 * r5 => 1
;; 4  eqrr 4 1 4      ; r4 = (= r1 r4) => not true (r1 = 10551309) => 0
;; 5  addr 4 2 2      ; add r4 to PC, move next while r3 * r5 != 10551309, else jump to 7
;; 6  addi 2 1 2      ; jump to 8
;; 7  addr 5 0 0      ; r0 = r0 + r5 = 1 + 1 => 2
;; 8  addi 3 1 3      ; r3 = 1 + r3 => 2
;; 9  gtrr 3 1 4      ; r4 = if r3 > r1 => 0 (r1 = 10551309)
;; 10 addr 2 4 2      ; nope, move next  (r2+=0)
;; 11 seti 2 8 2      ; jump to step 3
;; 12 addi 5 1 5      ; yes, r3 > r1, r5 = r5 + 1
;; 13 gtrr 5 1 4      ; is r5 > r1?
;; 14 addr 4 2 2      ; nope, move next 
;; 15 seti 1 5 2      ; jump to step 2
;; 16 mulr 2 2 2      ; jump to 256 aka terminate
;; 17 addi 1 2 1      ; r1 = r1 + 2  => 2 
;; 18 mulr 1 1 1      ; r1 = r1 * r1 => 4
;; 19 mulr 2 1 1      ; r1 = 19 * r1 => 76
;; 20 muli 1 11 1     ; r1 = r1 * 11 => 836
;; 21 addi 4 3 4      ; r4 = r4 + 3 => 3
;; 22 mulr 4 2 4      ; r4 = 22 * r4 => 66
;; 23 addi 4 7 4      ; r4 = r4 + 7 => 73
;; 24 addr 1 4 1      ; r1 = r1 + r4 => 836 + 73 => 909
;; 25 addr 2 0 2      ; r2 = 25 + r0 => 26 aka jump to 27
;; 26 seti 0 4 2      ; jump to 1                               [r1 = 909]
;; 27 setr 2 8 4      ; r4 = r2 => 27
;; 28 mulr 4 2 4      ; r4 = r4 * 28 => 756
;; 29 addr 2 4 4      ; r4 = r4 + 29 => 785
;; 30 mulr 2 4 4      ; r4 = r4 * 30 => 23550
;; 31 muli 4 14 4     ; r4 = r4 * 14 => 329700
;; 32 mulr 4 2 4      ; r4 = r4 * 32 => 10550400
;; 33 addr 1 4 1      ; r1 = r1 + r4 => 909 + 10550400 = 10551309
;; 34 seti 0 5 0      ; r0 = 0
;; 35 seti 0 8 2      ; jump to 0+1 = 1
