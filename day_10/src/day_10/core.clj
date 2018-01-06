(ns day-10.core (:gen-class))

(require '[clojure.string :as str])

(defrecord Hash-state [hash-seq offset skip])

(defn parse-int [s] (Integer/parseInt (re-find #"\A-?\d+" s)))

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
    (let [
        nums (:hash-seq hash-state)
    ]
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

(defn phase-one []
    (let [
        input (mapv parse-int (str/split (str/trim (slurp "input.txt")) #","))
    ]
        (def res (run-round (->Hash-state (range 256) 0 0) input))
        (reduce * (take 2 (roll (:hash-seq res) (:offset res))))
    )
)

(defn get-dense-hash [byte-seq]
    (mapv (fn [x] (reduce bit-xor x)) (partition 16 byte-seq))
)

(defn phase-two [input-str]
    (let [
        input (vec (concat (map int (seq input-str)) [17 31 73 47 23]))
        ]
        (def state (->Hash-state (range 256) 0 0))
        (dotimes [r 64]
            (def state (run-round state input))
        )
        (def dh (get-dense-hash (roll (:hash-seq state) (:offset state))))
        (apply str (map (fn [i] (format "%02x" i)) dh))
    )
)

(defn -main
    [& args]
    (println "Phase 1 result is     " (phase-one) )
    (println "Phase 2 result hash is" (phase-two (str/trim (slurp "input.txt"))))
)


; 1980
; 899124dac21012ebc32e2f4d11eaec55
