(ns day-16.core
	(:gen-class)
    (:require [clojure.string :as str])
)

(defn parse-int [s] (Integer/parseInt (re-find #"\A-?\d+" s)))

(defn spin [x progs]
    (into [] (apply concat (reverse (split-at (- (count progs) x) progs))))
)

(defn exchange [a b progs]
    (assoc progs a (progs b) b (progs a))
)

(defn partner [a b progs]
    (assoc progs (.indexOf progs a) b (.indexOf progs b) a)
)

(defn create-cmd [cmd-str]
    (case (first cmd-str)
        \s (partial spin (parse-int (subs cmd-str 1)))
        \x (apply partial exchange (map parse-int (str/split (subs cmd-str 1) #"/")))
        \p (apply partial partner (str/split (subs cmd-str 1) #"/")))
)

(defn dance-move [memory program]
    (def mem memory)
    (doseq [p program] (def mem (p mem)))
    mem
)

; Luckily, whole dance consists of few dozen moves (and not 16!) before it returns to original position. 
; After that it just loops...
(defn whole-dance [orig-vec program]
    (loop [i 1 new-vec (dance-move orig-vec program) res [orig-vec]]
        (if (= new-vec orig-vec) 
            res
            (recur (inc i) (dance-move new-vec program) (conj res new-vec))))
)

(defn -main
    [& args]
    (def prg (mapv create-cmd (str/split (str/trim (slurp "input.txt")) #",")))
    (def orig-vec (str/split "abcdefghijklmnop" #"")) ;"
    
    ; Part 1
    (def new-order (dance-move orig-vec prg))
    (println "Order after one move   :" (apply str new-order))

    ; Part 2
    (def dance-moves (whole-dance orig-vec prg))
    (println "Order after 10^9 moves :" (apply str (dance-moves (mod 1000000000 (count dance-moves)))))
)
;

; (load-file "src/day_16/core.clj")
; cknmidebghlajpfo
; cbolhmkgfpenidaj
