(ns day-05.core
  (:gen-class))


(require '[clojure.string :as str])

(defrecord State [ip ops])

; new ip = jump by instruction
; increment current jump instruction by 1
(defn phase-1-op [state]
    (def new-ip (+ (:ip state) (get (:ops state) (:ip state))))
    (def new-ops (assoc (:ops state) (:ip state) (inc (get (:ops state) (:ip state)))))
    (->State new-ip new-ops)
)

; new ip = jump by instruction
; increment current jump instruction by 1, unless it was three or more
(defn phase-2-op [state]
    (def jump (get (:ops state) (:ip state)))
    (def new-ip (+ (:ip state) jump))
    (def new-ops (assoc (:ops state) (:ip state) ((if (>= jump 3) dec inc) jump)))
    (->State new-ip new-ops)
)

(defn run [filename operator]
    (defn parse-int [s] (Integer/parseInt (re-find #"\A-?\d+" s)))
    (def ops (mapv parse-int (str/split (slurp filename) #"\n")))
    (def steps 0)
    (loop [state (->State 0 ops)]
        (def new-state (operator state))
        (def steps (inc steps))
        (if (>= (:ip new-state) (count ops))
            steps
            (recur new-state)
        )
    )
)

(defn -main
  [& args]
  (println "Phase 1 steps:" (run "input.txt" phase-1-op))
  (println "Phase 2 steps:" (run "input.txt" phase-2-op))
)