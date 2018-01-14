(ns day-19.core
    (:gen-class)
    (:require [clojure.string :as str])
)

(defn create-packet-map [filename]
    (let [packet-matrix (mapv (fn [r] (str/split r #"")) (str/split (slurp filename) #"\n"))]
        (defn create-row [r] (into {} (filter (fn [x] (not= (val  x) " ")) (zipmap (range (count r)) r))))
        (zipmap (range (count packet-matrix)) (map create-row packet-matrix)))
)

(defrecord Walker [x y heading])

(defn get-next-walker [walker packet-map]
    (let [
        [x y heading] [(:x walker) (:y walker) (:heading walker)]
        next-x (case heading :left (dec x) :right (inc x) x)
        next-y (case heading :up (dec y) :down (inc y) y)]
        (if (some? ((packet-map next-y) next-x)) (->Walker next-x next-y heading))
    )
)

(defn get-next-coordinate [walker packet-map]
    (let [
        [x y heading] [(:x walker) (:y walker) (:heading walker)]
        next-node (get-next-walker walker packet-map)]
        (if (some? next-node)
            next-node
            (case heading
                :up    (or (get-next-walker (->Walker x y :left) packet-map) (get-next-walker (->Walker x y :right) packet-map))
                :down  (or (get-next-walker (->Walker x y :left) packet-map) (get-next-walker (->Walker x y :right) packet-map))
                :right (or (get-next-walker (->Walker x y :up  ) packet-map) (get-next-walker (->Walker x y :down ) packet-map))
                :left  (or (get-next-walker (->Walker x y :up  ) packet-map) (get-next-walker (->Walker x y :down ) packet-map))
            )
        )
    )
)

(defn follow-path [packet-map]
    (loop [
        cw (->Walker (first (keys (packet-map 0))) 0 :down)
        cs ""
        steps 0
    ]
        (if (nil? cw)
            [cs steps]
            (recur 
                (get-next-coordinate cw packet-map) 
                (if (not (re-matches #"[-\|\+]" ((packet-map (:y cw)) (:x cw)))) ;"
                    (str cs ((packet-map (:y cw)) (:x cw)))
                    cs
                )
                (inc steps)
            )
        )
    )
)

(defn -main
    [& args]
    
    (apply printf "Part 1: %s\nPart 2: %d\n" (follow-path (create-packet-map "input.txt")))
)

; (load-file "src/day_19/core.clj")
; 1: UICRNSDOK
; 2: 16064