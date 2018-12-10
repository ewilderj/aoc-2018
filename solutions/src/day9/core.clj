(ns day9.core)

(defn rotate [v]
  (into (subvec v 2) (subvec v 0 2)))

(defn rback9 [v]
  (into (subvec v (- (count v) 9)) (subvec v 0 (- (count v) 9))))

(defn play [players max-marble]
  (loop [l [0] m 1 scores {}]
    (if (= 0 (mod m 1000)) (println "marble" m))
    (if (> m max-marble) (apply max (vals scores)) ; terminate game at max-marble
        (if (= 0 (mod m 23))
          (let [nl (rback9 l)
                winning (first nl)
                pn (mod m players)
                nsc (assoc scores pn (+ (scores pn 0) m winning))]
          ;; (println "player" pn "wins" m "and" winning "nl" nl)
            (recur (rotate (subvec nl 1))
                   (inc m) nsc)
            )
          ;; normal move
            (do ; (println "p#" (inc pn) "l" l "m" m(defn rotate [v]
              (recur
               (rotate (into [m] l))
               (inc m) scores)
            ))
        ))
    )

