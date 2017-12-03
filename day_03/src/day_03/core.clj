(ns day-03.core
    (:gen-class))

;; Phase 1 - find cell in spiral memory and calculate manhattan distance

; Get odd by odd square (3x3, 5x5, 7x7, ...)
(defn get-side-length [cell] 
    (def small-len (int (Math/ceil (Math/sqrt cell))))
    (if (= 0 (mod small-len 2))
        (+ 1 small-len)
        small-len
     )
)

; Map all cells to one side, calculate distance from side center and add distance to center
(defn get-distance [cell]
    (def side-length (get-side-length cell))
    (def max-point (int (Math/pow side-length 2)))
    (def min-point (+ 1 (- max-point (* 4 (- side-length 1)))))
    (def cell-idx (Math/abs (- (mod (- cell min-point) (- side-length 1)) (- (/ (- side-length 1) 2) 1))))
    (+ cell-idx (/ (- side-length 1) 2))
)

;; Part two: generate contents to spiral summing all neigbor cells to current cell

(defrecord Point [x y])

(defn sum-neighbors [mem point]
    (def x (:x point))
    (def y (:y point))
    (+
        (get mem (->Point (- x 1) (- y 1)) 0)
        (get mem (->Point    x    (- y 1)) 0)
        (get mem (->Point (+ x 1) (- y 1)) 0)
        (get mem (->Point (- x 1)    y   ) 0)
        (get mem (->Point (+ x 1)    y   ) 0)
        (get mem (->Point (- x 1) (+ y 1)) 0)
        (get mem (->Point    x    (+ y 1)) 0)
        (get mem (->Point (+ x 1) (+ y 1)) 0)
    )
)

; Next point deduced on where we are and where memory is initialized
(defn get-next-point [mem point]
    (def x (:x point))
    (def y (:y point))
    (def w (contains? mem (->Point (- x 1) y)))
    (def e (contains? mem (->Point (+ x 1) y)))
    (def n (contains? mem (->Point x (+ y 1))))
    (def s (contains? mem (->Point x (- y 1))))
    (cond
        (and w (not n)) (->Point    x    (+ y 1)) ; go north
        (and s (not w)) (->Point (- x 1)    y   ) ; go west
        (and e (not s)) (->Point    x    (- y 1)) ; go south
        (and n (not e)) (->Point (+ x 1)    y   ) ; go east
    )
)

; Walk spiral memory until neighborhood sum is large enough
(defn walk-spiral [until]
    (loop [mem {(->Point 0 0) 1} current-point (->Point 1 0)]
        (def n-sum (sum-neighbors mem current-point))
        (if (< n-sum until)
            (recur
                (assoc mem current-point n-sum)
                (get-next-point mem current-point)
            )
            n-sum))
)

(defn parse-int [s] (Integer/parseInt (re-find #"\A-?\d+" s)))

(defn -main
    [& args]
    (def cell (if (> (count args) 0) (parse-int (nth args 0)) 265149))
    (println "Phase 1: distance to cell" cell "is" (get-distance cell))
    (println "Phase 2: first value larger than" cell "in memory is" (walk-spiral cell))
)

;; 1: 438
;; 2: 266330
