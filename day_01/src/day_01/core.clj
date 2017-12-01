;;
;; AoC 2017 Day 1 solution
;;
;; My first Clojure code :)
;;

(ns day-01.core
  (:gen-class))

(require '[clojure.string :as str])

(defn to-digit [c] (Character/digit c 10))

(defn get-if-same [list i d]
    (if (= (nth list i) (nth list (mod (+ i d) (count list)))) (nth list i) 0))

(defn sum-list [list i d]
    (if (< i (count list))
        (+ (get-if-same list i d) (sum-list list (+ i 1) d))
        0))

(defn -main
  [& args]
  (def input (mapv to-digit (str/trim (slurp "input.txt"))))
  (println "Captcha is" (sum-list input 0 1))
  (println "Second Captcha is" (sum-list input 0 (/ (count input) 2)))
)
