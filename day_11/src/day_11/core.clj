(ns day-11.core (:gen-class))

(require '[clojure.string :as str])

;; ref https://www.redblobgames.com/grids/hexagons/
;; Using cube coordinates
(defrecord Coordinate [x y z])

;; Flat top hexes
(defn move [coord direction]
    (case direction
        "n"  (->Coordinate (:x coord) (inc (:y coord)) (dec (:z coord)))
        "ne" (->Coordinate (inc (:x coord)) (:y coord) (dec (:z coord)))
        "se" (->Coordinate (inc (:x coord)) (dec (:y coord)) (:z coord))
        "s"  (->Coordinate (:x coord) (dec (:y coord)) (inc (:z coord)))
        "sw" (->Coordinate (dec (:x coord)) (:y coord) (inc (:z coord)))
        "nw" (->Coordinate (dec (:x coord)) (inc (:y coord)) (:z coord))
    )
)

(defn distance [coord-1 coord-2]
    (/ (+ 
        (Math/abs (- (:x coord-1) (:x coord-2)))
        (Math/abs (- (:y coord-1) (:y coord-2)))
        (Math/abs (- (:z coord-1) (:z coord-2)))
    ) 2)
)

(defn get-distance [directions]
    (def coord (->Coordinate 0 0 0))
    (def max-dist 0)
    (doseq [dir directions]
        (def coord (move coord dir))
        (def max-dist (max max-dist (distance (->Coordinate 0 0 0) coord)))
    )
    [(distance (->Coordinate 0 0 0) coord) max-dist]
)

(defn -main
    [& args]
    (def directions (str/split (str/trim (slurp "input.txt")) #","))
    (def res (get-distance directions))
    (println "Distance to child is now" (res 0))
    (println "Maximum distance from origin was" (res 1))
)
