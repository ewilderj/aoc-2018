(ns day9.core)

(defn insert-at [l i p]
  (if (> p (count l))
    (do 
      (println "insert-at" p "for count" (count l))
      (conj l p))
  (into (conj (subvec l 0 p) i) (subvec l p))))

(defn remove-at [l p]
  (into (subvec l 0 p) (subvec l (inc p))))

(defn newpos [l p]
  (inc (mod (inc p) (inc (count l)))))

(defn play [players max-marble]
  (loop [pn 0 pos 1 l [0] m 1 scores {}]
    (if (= 0 (mod m 1000)) (println "marble" m))
    (if (> m max-marble) (apply max (vals scores)) ; terminate game at max-marble
        (if (= 0 (mod m 23))
          ;; multiple of 23
          (let [np (mod (- pos 9) (count l))
                winning (nth l np)
                nsc (assoc scores pn (+ (scores pn 0) m winning))]
 ;           (println "player" (inc pn) "wins" m "and" winning)
            (recur (mod (inc pn) players)
                   (inc (mod (inc np) (count l)))
                   (remove-at l np)
                   (inc m) nsc)
            )
          ;; normal move
 ;           (println "p#" (inc pn) "pos" pos "l" l "m" m)
            (recur (mod (inc pn) players)
                   (inc (mod (inc pos) (inc (count l))))
                   (insert-at l m pos)
                   (inc m) scores)
            )
        ))
    )

