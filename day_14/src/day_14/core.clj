(ns day-14.core
    (:gen-class)
    (:require [clojure.string :as str] clojure.set)
)

(defrecord Hash-state [hash-seq offset skip])

; (defn parse-int [s] (Integer/parseInt (re-find #"\A-?\d+" s)))

(defn knot [lst n]
    (let [parts (split-at n lst)]
        (concat (reverse (parts 0)) (parts 1)))
)

; roll list, positive ->, negative <-
(defn roll [lst n]
    (let [
        sn (mod (- (count lst) n) (count lst))
        parts (split-at sn lst)
    ]
        (concat (parts 1) (parts 0)))
)

(defn run-round [hash-state input]
    (let [nums (:hash-seq hash-state)]
        (def skip (:skip hash-state))
        (def current-pos-offset (:offset hash-state))
        (def byte-seq nums)
        (doseq [i input]
            ;; keep current position as first
            (def byte-seq (roll (knot byte-seq i) (* -1 (+ skip i))))
            (def current-pos-offset (mod (+ current-pos-offset skip i) (count nums)))
            (def skip (inc skip))
        )
        (->Hash-state byte-seq current-pos-offset skip)
    )
)

(defn get-dense-hash [byte-seq]
    (mapv (fn [x] (reduce bit-xor x)) (partition 16 byte-seq))
)

;; Kernighan's algorithm for bit counting
(defn count-set-bits [i]
    (loop [n i cnt 0]
        (if (= n 0)
            cnt 
            (recur (bit-and n (dec n)) (inc cnt))
        )
    )
)

(defn get-knot-hash [input-str]
    (let [
        input (vec (concat (map int (seq input-str)) [17 31 73 47 23]))
        ]
        (def state (->Hash-state (range 256) 0 0))
        (dotimes [r 64]
            (def state (run-round state input))
        )
        (get-dense-hash (roll (:hash-seq state) (:offset state)))
    )
)

(defn get-hashes [input-str] 
    (mapv get-knot-hash (map (fn [i] (str input-str i)) (range 128)))
)

(defn get-bit-counts [hashes] 
    (reduce + (map (fn [v] (reduce + (map count-set-bits v))) hashes))
)

; (defn to-bin-str [i] (str/replace (format "%1$8s" (Integer/toBinaryString i)) "0" " "))
; (defn get-str-bitmap [hashes]
;     (defn bits-to-str [i] (apply str (to-bin i)))
;     (mapv (fn [r] (apply str (map bits-to-str r))) hashes)
; )

(defn to-bin [i] (mapv (fn [b] (if (bit-test i b) 1 0)) (range 7 -1 -1)))

(defrecord Coordinate [x y])

(defn get-bitmap [hashes]
    (mapv (fn [a] (into [] (apply concat (mapv to-bin a)))) hashes)
)

(defn get-neighbors [x y mxi]
    (filter some?
        (list 
            (if (> y 0)   (->Coordinate x (dec y)))
            (if (> x 0)   (->Coordinate (dec x) y))
            (if (< y mxi) (->Coordinate x (inc y)))
            (if (< x mxi) (->Coordinate (inc x) y))
        ))
)

(defn get-on-neighbors [x y bitmap]
    (let [neighbors (get-neighbors x y (dec (count bitmap)))]
        (filter (fn [c] (= 1 ((bitmap (:y c))(:x c)))) neighbors)
    )
)

(defn is-on [coord bitmap]
    (= 1 ((bitmap (:y coord)) (:x coord)))
)

(defn get-connected [bitmap handled next-set]
    (if (empty? next-set)
        handled
        (do
            (def next-coord (first next-set))
            (def new-handled (conj handled next-coord))
            (def new-next-set (clojure.set/union (into #{} (rest next-set))
                (clojure.set/difference
                    (into #{} (get-on-neighbors (:x next-coord) (:y next-coord) bitmap))
                    handled))
            )
            (get-connected bitmap new-handled new-next-set)
        )
    )
)

(defn get-map [hashes]
    (let [bm (get-bitmap hashes)]
        (def group-id 0)
        (def ccoord (->Coordinate 0 0))
        (def handled-coordinates #{})
        (def groups {}) ; group-id -> #{coordinates}
        (doseq [y (range (count bm)) x (range (count (bm 0)))]
            (def ccoord (->Coordinate x y))
            (if (and (is-on ccoord bm) (not (contains? handled-coordinates ccoord))) 
                (do
                    (def group-id (inc group-id))
                    (def groups (conj groups {group-id (get-connected bm #{} #{ccoord})}))
                    (def handled-coordinates (clojure.set/union handled-coordinates (get groups group-id)))
                )
            )
        )
        groups
    )
)

(defn -main
      [& args]
      ; (println "Example:" (reduce + (map get-knot-hash (map (fn [i] (str "flqrgnkx-" i)) (range 128)))))
      (println "Computing 128 hashes...")
      (def hashes (get-hashes "hfdlxzhv-"))
      (println "Part 1, bit counts:" (get-bit-counts hashes))
      (println "Part 2, areas:" (count (get-map hashes)))
)

; 1: 8230
; 2: 1103
