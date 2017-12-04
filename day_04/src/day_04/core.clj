(ns day-04.core
  (:gen-class))

(require '[clojure.string :as str])

; Check duplicates on the row: go ahead one string a time and check that it is not found in rest of the row
; Return early (1) if duplicte is found
(defn check-dups [row]
  (if (< (count row) 2) 0
  (if (some #{(first row)} (rest row)) 1 (check-dups (rest row))))
)

; About the same as above, but sort each string's characters before comparison
(defn check-anagrams [row]
  (if (< (count row) 2) 0
  (if (some #{(sort (seq (first row)))} (map sort (map seq (rest row)))) 1 (check-anagrams (rest row))))
)

(defn -main
  [& args]
  (def data (mapv (fn [row] (str/split row #" ")) (str/split (str/trim (slurp "input.txt")), #"\n")))
  (def not-dups (count (filter (fn [n] (= n 0)) (map check-dups data))))
  (def not-anagram-dups (count (filter (fn [n] (= n 0)) (map check-anagrams data))))
  (println "Phase 1: there are" not-dups "valid passphrases")
  (println "Phase 2: there are" not-anagram-dups "valid non anagram passphrases")
)

;; 1: 337
;; 2: 231
