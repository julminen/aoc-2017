(ns day-13.core
    (:gen-class)
    (:require [clojure.string :as str])
)

(defn parse-int [s] (Integer/parseInt (re-find #"\A-?\d+" s)))

(defrecord Layer [depth range cycle])

(defn make-layer [depth range] 
    (->Layer depth range (+ range (- range 2))))


(defn mapgen [row]
    (let [entry (mapv parse-int (str/split row #": "))] ;"
        [(entry 0) (apply make-layer entry)]))

(defn get-damage [start-time layer-map]
    (defn dmg [ts layer] (if (= 0 (mod ts (:cycle layer))) (* (:depth layer) (:range layer)) 0))
    (reduce + (map dmg (map (fn [x] (+ x start-time)) (keys layer-map)) (vals layer-map)))
)

(defn is-caught [start-time layer-map]
    (defn dmg [ts layer] (= 0 (mod ts (:cycle layer))))
    (some true? (map dmg (map (fn [x] (+ x start-time)) (keys layer-map)) (vals layer-map)))
)

(defn get-zero-damage-delay [layer-map]
    ;; there is probably a smarter way to do this...
    (loop [ts 0]
        (if (is-caught ts layer-map) (recur (inc ts)) ts))
)

(defn -main
    [& args]
    (def layer-map (into {} (map mapgen (str/split (slurp "input.txt") #"\n"))))
    ; (def layer-map (into {} (map mapgen (str/split (slurp "example.txt") #"\n"))))

    (println "Damage if start at 0:" (get-damage 0 layer-map))
    (println "Zero damage if delayed for" (get-zero-damage-delay layer-map) "picoseconds")
)

; 648
; 3933124
