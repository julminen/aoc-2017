(ns day-07.core
    (:gen-class))

(require '[clojure.string :as str])

(defn parse-int [s] (Integer/parseInt (re-find #"\A-?\d+" s)))

(defrecord Program [name weight children])


(defn handle-row [row prg-map prg-tree]
    (let [
        parts (split-at 2 (filter (fn [x] (> (count x) 0)) (str/split row #"[^a-z0-9]"))) ; "
        prg (->Program (nth (nth parts 0) 0) (parse-int (nth (nth parts 0) 1)) (nth parts 1))
        ; prg-map: name -> program
        new-prg-map (assoc prg-map (:name prg) prg)
        ; prg-tree: program name -> parent program name
        new-prg-tree (merge prg-tree (zipmap (nth parts 1) (repeat (:name prg))))]
        [new-prg-map new-prg-tree]
    )
)

(defn get-root [tree]
    (loop [node (nth (keys tree) 0)]
        (let [parent (get tree node)]
            (if (= nil parent) node (recur parent))
        )
    )
)

(defn calculate-weight [nodename prg-map]
    (let [
        node (get prg-map nodename)]
        (reduce + (:weight node) (map (fn [n] (calculate-weight n prg-map)) (:children node)))
    )
)

(defn get-unbalanced-node [nodename prg-map]
    (let [
        subtotals (mapv (fn [n] (calculate-weight n prg-map)) (:children (get prg-map nodename)))
        sm (zipmap (:children (get prg-map nodename)) subtotals)
        diff-pair (first (filter (fn [x] (= (val x) 1)) (frequencies (vals sm))))]
        (if (some? diff-pair)
            (first (filter (fn [x] (= (val x) (key diff-pair))) sm))
            nil)
    )
)

(defn get-new-node-weight [nodename prg-map prg-tree]
    (let [
        node (get prg-map nodename)
        siblings (filter (fn [n] (not (= n nodename))) (:children (get prg-map (get prg-tree nodename))))
        child-weight (- (calculate-weight nodename prg-map) (:weight node))
        target-weight (calculate-weight (first siblings) prg-map)]
        (- target-weight child-weight)
    )
)

(defn find-bad-node [nodename prg-map prg-tree]
    (let [next-node (get-unbalanced-node nodename prg-map)]
        (if (some? next-node)
            (find-bad-node (key next-node) prg-map prg-tree)
            (->Program nodename (get-new-node-weight nodename prg-map prg-tree) nil)
        )
    )
)

(defn -main
    [& args]
    (def filename (if (> (count args) 0) (nth args 0) "input.txt"))
    (def file (str/split (slurp filename) #"\n"))
    (def prg-map {})
    (def prg-tree {})
    (doseq [row file]
        (def res (handle-row row prg-map prg-tree))
        (def prg-map (nth res 0))
        (def prg-tree (nth res 1))
    )
    (def root-node (get-root prg-tree))
    (def unbalanced-node (find-bad-node root-node prg-map prg-tree))
    (println "Root node is" root-node)
    (println "Unbalanced node is" (:name unbalanced-node) ". Its weight should be" (:weight unbalanced-node))
)


; root: svugo
; 1152