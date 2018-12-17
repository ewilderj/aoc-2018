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
 ;;          (println "player" pn "wins" m "and" winning "nl" nl)
            (recur (rotate (subvec nl 1))
                   (inc m) nsc)
            )
          ;; normal move
          (do
 ;;            (println "l" l "m" m)
              (recur
               ;;    (rotate (into [m] l))
               (conj (subvec l 1) m (first l))
               (inc m) scores)
            ))
        ))
    )

;; 410 players, marble 72059 -> 429287
;; 410 players, marble 7205900 -> 3624387659

;; let's make a circular buffer where it's relatively efficient
;; to rotate

;; add a marble and rotate onward
(defn craddl [c i]
  {:l (conj (subvec (c :l) 1) i (first (c :r)))
   :r (conj (subvec (c :r) 1) (first (c :l)))})

;; drop the marble from 9 back, and rotate on 2
(defn cdandr [c]
  (let [l (c :l)
        n (- (count l) 9)]
    {:l (into (subvec l 1 n) (subvec l (inc n) (+ 3 n)))
     :r (conj (into (subvec l (+ n 3)) (c :r)) (first l))}))

;; the winning marble is 9 back from the end
(defn win9 [c]
  (nth (c :l) (- (count (c :l)) 9)))

(defn cplay [players max-marble]
  (loop [c {:l [0] :r [1]} m 2 scores {}]
    (if (= 0 (mod m 10000)) (println "marble" m))
    (if (> m max-marble) (apply max (vals scores)) ; terminate game at max-marble
        (if (= 0 (mod m 23))
          (let [pn (mod m players)]
            ;;(println "player wins" m "and" (win9 c) "nc" (cdandr c))
            (recur (cdandr c)
                   (inc m)
                   (assoc scores pn (+ (scores pn 0) m (win9 c)))))
          ;; normal move
          (do
            ; (println "c" c "m" m)
            (recur
             (craddl c m)
             (inc m) scores)
            ))
        ))
  )

;; version which uses something like a linked list

(def lring {0 {:n 0 :p 0}})

(defn add-after [ring where item]
  (let [n ((ring where) :n)
        p ((ring where) :p)]
    (-> ring
        (assoc-in [where :n] item)
        (assoc-in [n :p] item)
        (assoc-in [item :n] n)
        (assoc-in [item :p] where))
    ))

(defn ring-remove [ring item]
  (let [p ((ring item) :p)
        n ((ring item) :n)]
    (-> (dissoc ring item)
        (assoc-in [p :n] n)
        (assoc-in [n :p] p)
        )
    )
  )

(defn back-n [ring item n]
  (if (<= n 0) item
      (recur ring (get-in ring [item :p]) (dec n))))

(defn trav [ring item]
  (loop [r ring i item l []]
    (if (empty? r) l
        (recur (dissoc r i)
               (get-in r [i :n])
               (conj l i))
        )
    ))

(defn rplay [players max-marble]
  (loop [c lring m 1 z 0 scores {}]
    ;; (if (= 0 (mod m 1000)) (println "marble" m))
    (if (> m max-marble) (apply max (vals scores)) ; terminate game at max-marble
        (if (= 0 (mod m 23))
          (let [pn (mod m players)
                winning (back-n c z 8)
                nc (ring-remove c winning)]
;;            (println "player wins" m "and" winning "nc" (trav nc 0))
            (recur nc
                   (inc m)
                   (back-n c z 6)
                   (assoc scores pn (+ (scores pn 0) m winning))))
          ;; normal move
          (do
;;            (println "c" (trav c 0) "m" m)
            (recur
             (add-after c z m)
             (inc m)
             (get-in c [z :n])
             scores)
            ))
        ))
  )
