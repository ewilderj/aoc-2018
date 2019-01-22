(ns day21.core
  (:require [aoc.core :as aoc] [clojure.string :as str]
            [clojure.set :as set] [clojure.spec.alpha :as s]))

(def inp (aoc/puzzle-lines "day21"))
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
  (loop [c (assoc-in (make-computer i) [:r 0] r0)
         ticks 0
         lowest Integer/MAX_VALUE]
    ;; (if (= 0 (mod ticks 10000))
    ;;   (printf "0x%8x\n" (get-in c [:r 4])))

    (let [lowest' 
          (if (= (ip c) 21)
            (let [nl (min (get-in c [:r 4]) lowest)]
              (println (c :r) "t" ticks "L" nl)
              nl
              )
            lowest)]

      (if-let [ops (get-in c [:prog (ip c)])] ;; if there's a next instruction
        (recur (inc-ip (execute c ops)) (inc ticks) lowest')
        (assoc (dec-ip c) :ticks ticks)))))

;; part1 answer is 15823996
;; determined this by halting the program at PC 28 and printing r4

(defn part1 [i] (get-in (run i 0) [:r 0])) ;; => 1326

;; #ip 1
;; 00  seti 123 0 4           ; 123 -> r4
;; 01  bani 4 456 4           ; 123 & 456 -> r4
;; 02  eqri 4 72 4            ; if 72 (working right), 1 -> r4
;; 03  addr 4 1 1             ; add 1 to ip1 (PC), ie. skip next
;; 04  seti 0 0 1             ; jump to 0 (skipped if above works)
;; 05  seti 0 8 4             ; 0 -> r4
;; 06  bori 4 65536 3         ; r4|0x10000 -> r3 (65536) 
;; 07  seti 16098955 8 4      ; 16098955 -> r4
;; 08  bani 3 255 5           ; take bottom 8 bits of r3, -> r5 (0)
;; 09  addr 4 5 4             ; r4 += r5
;; 10  bani 4 16777215 4      ; r4 & 0xffffff -> r4 (bottom 24 bits)
;; 11  muli 4 65899 4         ; r4 * 0x1016b -> r4
;; 12  bani 4 16777215 4      ; take bottom 24 bits again
;; 13  gtir 256 3 5           ; if r3 <= 256...
;; 14  addr 5 1 1             ; skip to 16
;; 15  addi 1 1 1             ; else skip to 17
;; 16  seti 27 3 1            ; jump to 28  (r3 <= 256)
;; 17  seti 0 7 5             ; 0 -> r5
;; 18  addi 5 1 2             ; r5 + 1 -> r2 ie. 1 -> r2
;; 19  muli 2 256 2           ; r2 << 8 -> r2 ie. 256 -> r2
;; 20  gtrr 2 3 2             ; if r2 > r3 ... (256 > 65536 ?)
;; 21  addr 2 1 1             ; then skip to 23
;; 22  addi 1 1 1             ; skip to 24
;; 23  seti 25 1 1            ; skip to 26 (if r2 > r3)
;; 24  addi 5 1 5             ; (r2 <= r3) r5 = r5 + 1
;; 25  seti 17 6 1            ; jump to 18 - last loop exec has r5 = 256
;; 26  setr 5 4 3             ; copy r5 to r3 (256)
;; 27  seti 7 5 1             ; jump to 08
;; 28  eqrr 4 0 5             ; is r4 = r0? 
;; 29  addr 5 1 1             ; yes, jump to 31 (HALT)
;; 30  seti 5 3 1             ; jump to 06, start again


;; part 2 asks--
;; What is the lowest non-negative integer value for register 0 that causes the
;; program to halt after executing the most instructions?
;;
;; ie. figure out what would cause this program never to halt
