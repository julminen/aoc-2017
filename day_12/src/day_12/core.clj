(ns day-12.core (:gen-class)
    (:require [clojure.string :as str] clojure.set)
)

(defn parse-int [s] (Integer/parseInt (re-find #"\A-?\d+" s)))

;; Map proc_id -> Set of directly visible prog ids

(defn mapgen [row]
    (let [
        entry (mapv parse-int (filter (fn [s] (> (count s) 0)) (str/split row #"[ <\->,]")))] ;"
        [(first entry) (into #{} (rest entry))]
    )
)

(defn get-connected-nodes [prg-map prg-id]
    (let []
        (def handled-nodes #{prg-id})
        (def potential-nodes (get prg-map prg-id))
        (while (seq potential-nodes)
            (def next-node (first potential-nodes))
            (def potential-nodes (rest potential-nodes))
            (when-not (contains? handled-nodes next-node)
                (def potential-nodes (clojure.set/union potential-nodes (get prg-map next-node)))
                (def handled-nodes (conj handled-nodes next-node))
            )
        )
        handled-nodes
    )
)

(defn get-disconnected-groups [prg-map]
    (let []
        (def seen-nodes #{})
        (def node-groups #{})
        (doseq [prg-id (keys prg-map)]
            (if-not (contains? seen-nodes prg-id)
                (let [new-group (get-connected-nodes prg-map prg-id)]
                    (def node-groups (conj node-groups new-group))
                    (def seen-nodes (clojure.set/union (conj seen-nodes prg-id) new-group))
                )
            )
        )
        node-groups
    )
)


(defn -main
    [& args]
    (def prog-map (into {} (map mapgen (str/split (slurp "input.txt") #"\n"))))
    (println "Connected to node 0:" (count (get-connected-nodes prog-map 0)) "nodes")
    (println "Disconnected group count:" (count (get-disconnected-groups prog-map)))
)

; 283
; 195