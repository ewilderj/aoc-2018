(ns day7.core
  (:require [aoc.core :as aoc] [clojure.string :as str]
            [clojure.set :as set]))

(def test-input "Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.
")

(defn parse-line [l] (map first (rest (re-find #"tep (\w).*tep (\w)" l))))

(def t (map parse-line (str/split-lines test-input)))
(def inp (map parse-line (aoc/puzzle-lines "day7")))

(defn all-nodes [v] (sort (distinct (flatten v))))

(defn things-before [v a]
  (->> v (filter #(= a (second %))) (map first) (apply sorted-set)))

(def all-things-before
  (memoize
   (fn [v a]
     (let [x (things-before v a)]
       (set/union x (apply set/union 
                           (map #(all-things-before v %) x)))))))

(defn available-moves [v m]
  (let [ms (set m)]
    (remove ms (filter #(set/superset? ms (all-things-before v %))
                       (all-nodes v)))))

(defn process [v]
  (loop [m []]
    (let [a (available-moves v m)]
      (if (empty? a) m (recur (conj m (first a)))))))

(def part1 (apply str (process inp))) ; "JMQZELVYXTIGPHFNSOADKWBRUC"

;; part 2
(defn recv-move [c] [c (- (int c) 4)])

(def max-workers 5)

(defn active-tasks [w] (set (map first w)))

(defn fill-workers [workers v m]
  (loop [w workers]
    (if (>= (count w) max-workers) w
        (let [a (remove (active-tasks w) (available-moves v m))]
          (if (empty? a) w
              (recur (conj w (recv-move (first a)))))))))

(defn clock-tick [w m t]
  (let [nw (for [[tw tt] w] [tw (dec tt)])
        d (map first (filter #(= 0 (second %)) nw))
        rw (filter #(> (second %) 0) nw)]
    [rw (concat m d) (inc t)]))

(defn machine [v]
  (loop [[w m t] [[] [] 0]]
    (let [tw (fill-workers w v m)]
      (if (empty? tw) t (recur (clock-tick tw m t))))))

(def part2 (machine inp)) ; 1133
