(ns day-15.core
    (:gen-class)
)

; First phase generators
(defn genA [s] (mod (* s 16807) 2147483647))
(defn genB [s] (mod (* s 48271) 2147483647))

; Second phase generators (recursive)
(defn genA2 [s] 
    (let [cv (mod (* s 16807) 2147483647)]
        (if (= 0 (mod cv 4)) cv (genA2 cv)))
)
(defn genB2 [s] 
    (let [cv (mod (* s 48271) 2147483647)]
        (if (= 0 (mod cv 8)) cv (genB2 cv)))
)

; Check if lowest 16 bits match, return 1 if they do and 0 otherwise
(defn judge [a b] (if (= (bit-and a 0xffff) (bit-and b 0xffff)) 1 0))

; Check two generator outputs, return number of matching lowest 16 bits
(defn check-generators [a-seed b-seed samples generatorA generatorB]
    (reduce + 
        (map judge 
            (take samples (rest (iterate generatorA a-seed))) 
            (take samples (rest (iterate generatorB b-seed)))))
)

(defn -main
    [& args]
    (println "Computing....")
    ; Input values for generators A and B are 783 and 325
    (println "Part 1: " (check-generators 783 325 40000000 genA genB))
    (println "Part 2: " (check-generators 783 325 5000000 genA2 genB2))
)
  
; Reload: (load-file "src/day_15/core.clj")

; Phase 1: 650
; Phase 2: 336
