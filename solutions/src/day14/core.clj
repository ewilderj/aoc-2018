(ns day14.core
  (:require [aoc.core :as aoc] [clojure.string :as str]
            [clojure.set :as set]))

(defn new-recipes [score]
  (->> score
       (str)
       (map int)
       (map #(- % 48))))

(defn play [max-r]
  (loop [recipes [3 7] elf1 0 elf2 1]
    ;; (println recipes elf1 elf2)
    (if (>= (count recipes) (+ 10 max-r))
      (apply str (take 10 (drop max-r recipes)))
      (let [s1 (nth recipes elf1) s2 (nth recipes elf2)
            sc (+ s1 s2)
            nr (into recipes (new-recipes sc))
            nn (count nr)
            ]
        (recur nr
               (mod (+ 1 s1 elf1) nn)
               (mod (+ 1 s2 elf2) nn))
      )
    )
  ))

;; part1: (play 556061) => "2107929416"

(defn play2 [target]
  (let [tseq (new-recipes target)]
    (loop [recipes [3 7] elf1 0 elf2 1]
      (if (>= (java.util.Collections/indexOfSubList recipes tseq) 0)
        (java.util.Collections/indexOfSubList recipes tseq)
        (let [s1 (nth recipes elf1) s2 (nth recipes elf2)
              sc (+ s1 s2)
              nr (into recipes (new-recipes sc))
              nn (count nr)
              ]
          (recur nr
                 (mod (+ 1 s1 elf1) nn)
                 (mod (+ 1 s2 elf2) nn))
      ))
    )
  ))

;; part2: (play2 556061) =>

