(ns day-17.core
    (:gen-class))

(defn insert-at [elements index element]
    (let [[before after] (split-at index elements)]
        (vec (concat before (list element) after)))
)

(defrecord State [memory position round])

(defn new-state [state steps]
    (let [
        mem-count (count (:memory state))
        new-index (inc (mod (+ steps (:position state)) mem-count))
        new-mem (insert-at (:memory state) new-index (inc (:round state)))]
        ; (println "mc" mem-count "ni" new-index "nm" new-mem)
        (->State new-mem (mod new-index (inc mem-count)) (inc (:round state)))
    )
)

(defn new-reduced-state [state steps]
    (let [
        mem-count (inc (:round state))
        new-index (inc (mod (+ steps (:position state)) mem-count))
        ; new-mem (insert-at (:memory state) new-index (inc (:round state)))
        new-mem (if (= 1 new-index) [0 mem-count] (:memory state)) 
    ]
        ; (println "mc" mem-count "ni" new-index "nm" new-mem)
        (->State new-mem (mod new-index (inc mem-count)) (inc (:round state)))
    )
)

(defn part-1 [input]
    (defn news [x] (new-state x input))
    (let [last-state (nth (iterate news (->State [0] 0 0)) 2017)]
        ((:memory last-state) (mod (inc (:position last-state)) (count (:memory last-state)))))
)

(defn part-2 [input]
    (defn news [x] (new-reduced-state x input))
    ((:memory (nth (iterate news (->State [0] 0 0)) 50000000)) 1)
)

(defn -main
    [& args]
    (println "Part 1:" (part-1 354))
    (println "Part 2:" (part-2 354))
)


; (load-file "src/day_17/core.clj")
; 1: 2000
; 2: 10242889
