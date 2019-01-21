(ns day20.core
  (:require [aoc.core :as aoc] [clojure.string :as str]
            [clojure.set :as set] [clojure.spec.alpha :as s]
            [instaparse.core :as insta]))

(def mazeland
  (insta/parser
   "s = (('N' | 'E' | 'W' | 'S') | c)+
    c = <'('> (s | ((s '|')+ s?)) <')'>"))

(def inp
  (let [s (aoc/puzzle-data "day20")
        c (count s)]
    (subs s 1 (- c 2))))

(defn remove-loops
  "Removes loops of form (NEWS|)"
  [s]
  (apply str (apply concat (str/split s #"\(\w+\|\)"))))

(def ex "WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))")

(defn process [s])
(defn process-choice [s]
  ;; when we get here we have a list of alternates separated by |
  ;; if our terminal is a "|" then matching empty is an option
  (if (= (peek s) "|") 0
      (let [p (partition 2 (conj s "|"))
            pairs (apply concat (map drop-last p)) ;; get rid of |
            pp (map process pairs)         ;; process each option
            pt (map (partial apply +) pp)] ;; sum each option
        (apply max pt))))                  ;; take the longest

(defn process [s]
  (loop [s s r []]
    (if (empty? s) r
        (let [i (first s)]
          (cond
            (= i :s)     ;; sequence, so let's count all what's next
            (recur (subvec s 1) r)
            (= i :c)     ;; we have a choice on our hands
            (process-choice (subvec s 1))
            (vector? i)  ;; either a choice or another sequence
            (recur (subvec s 1) (conj r (process i))) ;; process the vec, move on
            :else        ;; it should just be a char
            (recur (subvec s 1) (conj r 1))
            )))))

(defn part1 [s] (apply + (process (mazeland s))))

(defn process2 [s])
(defn process-choice2 [s]
  ;; when we get here we have a list of alternates separated by |
  ;; if our terminal is a "|" then matching empty is an option
  ;; and we move on, because we're looking for shortest routes
  (if (= (peek s) "|") nil
      (let [p (partition 2 (conj s "|"))
            pairs (apply concat (map drop-last p))]  ;; get rid of |
        (apply list (map process2 pairs)))))         ;; process each option

(defn process2 [s]
  (loop [s s r []]
    (if (empty? s)
      (if (= (count r) 1) (first r) r)
      (let [i (first s)]
        (cond
          (= i :s)     ;; sequence, so let's count all what's next
          (recur (subvec s 1) r)
          (= i :c)     ;; we have a choice on our hands
          (process-choice2 (subvec s 1))
          (vector? i)  ;; either a choice or another sequence
          (recur (subvec s 1)  ;; process the vec, move on
                 (if-let [x (process2 i)] (conj r x) r))
          :else        ;; it should just be a char
          (recur (subvec s 1)
                 (if (number? (peek r)) ;; keep summing a char sequence
                   (conj (pop r) (inc (peek r)))
                   (conj r 1)))         ;; start a new char sequence
          )))))

;; by this point, we have a data structure where there are
;; sequences of steps, with choices represented by lists ()
;;
;; > (process2 (mazeland "E((EE|EEE)|EEEE)"))
;; [1 ((2 3) 4)]

(declare z)

(defn fatcat
  "Flattens a list but leaves vectors intact."
  [x]
  (let [f (fn [x] (and (not (vector? x)) (sequential? x)))
        r (filter (complement f)
                  (tree-seq f identity x))]
    r))

(defn z-choice
  "Process a choice: returns a list of every fully expanded choice."
  [s]
  (let [q (map z s)
        f (fatcat q)]
    ;; (println "z-choice" s "-+" q "f" f)
    f
    ))

(defn z [s]
  (cond
    (number? s) s
    (list? s) (z-choice s)
    :else ;; vector, iterate through and process each item
      (loop [s s r [[]]]
        ;; (println "s" s "r" r "i" (first s))
        (if (empty? s) r
            (let [i (first s)]
              (cond
                (list? i)
                ;; this is a choice
                ;; resolve and expand all the choices; then for each
                ;; create a perm of each element in our existing return r
                (let [q (z-choice i)
                      r' (apply concat
                                     (for [j r] (apply list (map #(conj j %) q))))]
                  ;; (println i "=>" q "r" r "r'" r')
                  (recur (vec (next s)) r')) ;; process next in sequence

                (vector? i)
                ;; this is a sequence; process it, and
                ;; append the returned seq to everything in r
                (recur (vec (next s))
                       (map #(concat % (z i)) r))

                :else ;; base case, it's a number
                (recur (vec (next s))
                       (map #(conj % i) r))) ;; append the number to everything 

              )))))

(defn part1' [x]
  (->> (z (process2 (mazeland x)))
       (map flatten)
       (map (partial apply +))
       (apply max)))

;; -> 4308

(defn thousand-or-more [s]
  (loop [s s t 0]
    (cond
      (>= t 1000) (inc (count s))
      (empty? s) 0
      :else
      (recur (rest s) (+ t (first s)))
      )))

(defn part2 [x]
  (->> (z (process2 (mazeland x)))
       (map flatten)
       (map thousand-or-more)
       ))

;; guessed 663, too low
;; the total number of routes was 761, also too low.
;; given that there are 760 pipes and 749 open parens
;; in the (remove-loops inp) then we should expect
;; significantly more loops. clearly something is collapsing
;; routes.

;; second exploration udnerstands the problem fine
;; by figuring out how many stops there are after
;; we reach 1000 steps. however that's 77023 which
;; means we actualyl need to keep track of rooms to
;; avoid counting for rooms we see a lot - the problem
;; states the shortest path. looks like the right answer
;; is ~5000-8000 from other people's experience.

;; third try

(def delta {\N [0 -1] \S [0 1] \E [1 0] \W [-1 0]})
(defn move [p d] (mapv + p (delta d)))

(def BIGNUM Integer/MAX_VALUE)

(defn visit-room [rooms p d]
  (if (< d (get rooms p BIGNUM)) (assoc rooms p d) rooms))

(defn accumulate [a p]
  (conj (pop a) (conj (peek a) p)))

(defn update-rooms [rooms new-rooms]
  (let [ks (distinct (concat (keys rooms) (keys new-rooms)))]
    (reduce (fn [rs k] (visit-room rs k (get new-rooms k BIGNUM))) rooms ks)
    )
  )

(defn play
  "Run the simulation - s is the regexp string
  rooms is the original state of rooms and the distance to them
  h is a history of state when a choice is encountered
  p is the starting position
  d is the starting distance
  a is a history of accumulated choices"
  ([s] (play (remove-loops s) {[0 0] 0} [] [0 0] [] {}))
  ([s rooms h p a]
   (loop [s s rooms rooms h h p p a a seen seen]
     (if (or (empty? s) (seen [p s])) rooms ;; avoid repeating
         (let [i (first s) s' (rest s)]
           (cond
             (#{\N \E \W \S} i) ;; direction
             (let [p' (move p i)
                   rooms' (visit-room rooms p' (inc (rooms p)))]
               (recur s' rooms' h p' a (assoc seen [p s] true)))

             (= i \() ;; open choice
             ;; remember starting point, start a fresh accumulator
             (recur s' rooms (conj h p) p (conj a []) seen)

             (= i \|) ;; choice delimiter
             ;; push our end state into the accumulator
             ;; then restart again with the start state
             (let [p' (peek h)
                   a' (accumulate a p)]
               ;; (println "Added choice" a')
               (recur s' rooms h p' a' seen))

             (= i \))
             ;; add final choice, and then...
             ;; for every ending position in the accumulator
             ;; play out how it might end from there
             (let [a' (accumulate a p)]
               (recur s' rooms (pop h) (peek h) (pop a') seen)
                     )
           )
         )))))



;; (let [a' (accumulate a p)]
;;   (loop [sa (peek a') rooms' rooms]
;;     (if (empty? sa) rooms'
;;         (let [sp (first sa)]
;;           (recur (rest sa)
;;                  (play s' rooms'
;;                        (pop h) sp (pop a') seen))
;;           )
;;         )))



;; (let [a' (accumulate a p)]
;;   (recur s' rooms
;;          (pop h) (peek h) (pop a'))
;;   )


;; we're looking for 8528
