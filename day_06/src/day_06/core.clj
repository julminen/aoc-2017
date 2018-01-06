(ns day-06.core
  (:gen-class))

(require '[clojure.string :as str])

(defn parse-int [s] (Integer/parseInt (re-find #"\A-?\d+" s)))

(defn redistribute [mem]
    (def max-value (apply max mem))
    (def max-index (.indexOf mem max-value))
    (def min-add (int (/ max-value (count mem))))
    (def mem-count (count mem))
    
    (defn remain [index]
        (def idx (mod (- index max-index) mem-count))
        (if (and (> idx 0) (<= idx (mod max-value mem-count) mem-count)) 1 0)
    )
    (def new-mem [])
    (dotimes [index (count mem)]
        (def new-val (+ (if (= max-index index) 0 (nth mem index)) min-add (remain index)))
        (def new-mem (conj new-mem new-val))
    )
    new-mem
)

(defn find-loop [state]
    (loop [history #{state} m state steps 1]
        (def new-mem (redistribute m))
        (if (not (contains? history new-mem))
            (recur (conj history new-mem) new-mem (inc steps))
            (println "Did " steps " steps, state already seen: " new-mem)
        )
    )
    new-mem
)

(defn -main
    [& args]
    (def filename (if (> (count args) 0) (nth args 0) "input.txt"))
    (def mem (mapv parse-int (str/split (slurp filename) #"\t")))
    (def phase-1 (find-loop mem))
    (println phase-1)
    (find-loop phase-1)
)
