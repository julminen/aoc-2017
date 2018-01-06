(ns day-08.core (:gen-class))

(require '[clojure.string :as str])

(defn parse-int [s] (Integer/parseInt (re-find #"\A-?\d+" s)))

(defrecord Instruction [mod-reg mod-op mod-val cond-reg cond-op cond-val])

(defn create-instruction [row]
    (let [parts (str/split row #" ")] ;"
        (->Instruction 
            (parts 0)
            (case (parts 1) "inc" + "dec" -)
            (parse-int (parts 2))
            (parts 4)
            (case (parts 5)
                "<" <
                "<=" <=
                ">" >
                ">=" >=
                "==" =
                "!=" not=
            )
            (parse-int (parts 6)))
    )
)

(defn load-program [filename]
    (mapv create-instruction (str/split (slurp filename) #"\n"))
)

(defn execute-instruction [instruction memory]
    (if 
        ((:cond-op instruction) (get memory (:cond-reg instruction) 0) (:cond-val instruction))
        (assoc memory (:mod-reg instruction) 
            ((:mod-op instruction) (get memory (:mod-reg instruction) 0) (:mod-val instruction)))
        memory
    )
)

(defn -main
    [& args]
    (def filename (if (> (count args) 0) (nth args 0) "input.txt"))
    (def program (load-program filename))
    (def mem {})
    (def max-val 0)
    (doseq [instruction program]
        (def mem (execute-instruction instruction mem))
        (def max-val (max max-val (apply max (vals mem))))
    )
    (println "Max register value is" (apply max (vals mem)))
    (println "Max register value ever was" max-val)
)
