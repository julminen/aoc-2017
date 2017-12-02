(ns day-02.core
  (:gen-class))

(require '[clojure.string :as str])

; Read input file: Rows are separated by newline and values by tab
; Return vector of vectors
(defn process-input [filename]
  (defn parse-int [s] (Integer/parseInt (re-find #"\A-?\d+" s)))
  (defn process-row [row] (mapv parse-int (str/split row #"\t")))
  (def rows (str/split (str/trim (slurp filename)), #"\n"))
  (mapv process-row rows))

; First row checksum: add smallest value on row to biggest value
(defn row-checksum-1 [row] 
  (- (apply max row) (apply min row)))

; Second row checksum: on row, find where one value is divisible by another 
; value on same row and return result of that calculation.
; :( No bonus points here
(defn row-checksum-2 [row]
  ; get all permutations, filter those where division is sensible and check for modulo
  (reduce + (for [a row b row :when (and (> a b) (= (mod a b) 0))] (/ a b))))

(defn -main
  [& args]
  (def rows (process-input "input.txt"))
  (println "Checksum 1: " (apply + (map row-checksum-1 rows)))
  (println "Checksum 2: " (apply + (map row-checksum-2 rows))))

;; Answers:
;; 1 : 48357
;; 2 : 351
