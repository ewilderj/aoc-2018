(ns day4.core
  (:require [aoc.core :as aoc] [clojure.string :as str]
            [clojure.set :as set]))

;; preprocess data into [guard, sleep-session] pairs
(def sleep-info (->> (aoc/puzzle-lines "day4")
            (sort-by #(subs % 1 17))                   ; timestamp sort
            (partition-by #(str/includes? % "Guard"))  ; split on new guard
            (partition 2)
            ))

;; parsing minute; always same char pos
(defn get-minute [s] (Integer. (subs s 15 17)))

(defn all-minutes [[g i]]
  "List out all the minutes a guard is asleep that night"
  (let [guard (Integer. (second (re-find #"#(\d+)" (first g))))
        sleep-data (->> i
                (map get-minute)                       ; parse times
                (partition 2)                          ; wake/sleep pairs
                (map #(apply range %))                 ; expand into each minute
                (flatten))]
        [guard sleep-data]                             ; guard/mins tuple
    ))

(defn aggregate-minutes [data]
  "Collect all the minutes each guard is asleep"
  (reduce (fn [t [guard mins]] (assoc t guard (concat (t guard []) mins)))
          {} (map all-minutes data)))

(defn sleepiest-minute [gmins]
  "Minute/frequency pair for the most sleepy minute"
  (last (sort-by second (seq (frequencies (second gmins))))))

(def part1
  (let [s (aggregate-minutes sleep-info)           ; aggregate all the sleep
        gd (last (sort-by #(count (second %)) s))] ; guard with the most mins
    (* (first gd) (first (sleepiest-minute gd)))))

(def part2 
  (->> sleep-info
       (aggregate-minutes)
       (reduce #(assoc %1 (first %2) (sleepiest-minute %2)) {}) ; map guards to most consistent minute
       (sort-by (comp second second))            ; sort by frequency of minute
       (last)                                    ; most consistent minute
       (apply #(* %1 (first %2)))))              ; multiply guard id x minute
