(ns day-18.core
    (:gen-class)
    (:require [clojure.string :as str])
)

(defn queue
    ([] clojure.lang.PersistentQueue/EMPTY)
    ([coll] (reduce conj clojure.lang.PersistentQueue/EMPTY coll))
)

(defmethod print-method clojure.lang.PersistentQueue
    [q ^java.io.Writer w]
    (.write w "#queue ")
    (print-method (sequence q) w)
)

(defrecord Program [memory pc snd rcv])

(defn parse-int [s] (Integer/parseInt (re-find #"\A-?\d+" s)))

(defn get-value [r-name prg]
    (if (re-matches #"^-?\d+$" r-name)
        (parse-int r-name)
        (get (:memory prg) r-name 0))
)

(defn int-set [x y prg]
    (->Program (assoc (:memory prg) x y) (inc (:pc prg)) (:snd prg) (:rcv prg))
)

(defn i-set [x y prg]
    (int-set x (get-value y prg) prg)
)

(defn i-add [x y prg]
    (int-set x (+ (get-value x prg) (get-value y prg)) prg)
)

(defn i-mul [x y prg]
    (int-set x (* (get-value x prg) (get-value y prg)) prg)
)

(defn i-mod [x y prg]
    (int-set x (mod (get-value x prg) (get-value y prg)) prg)
)

(defn i-jgz [x y prg]
    (let [
        x (get-value x prg) 
        y (get-value y prg)
        new-pos (if (> x 0) (+ y (:pc prg)) (inc (:pc prg)))
    ]
        (->Program (:memory prg) new-pos (:snd prg) (:rcv prg))
    )
)

(defn i-snd [x prg] 
    (->Program (:memory prg) (inc (:pc prg)) (get-value x prg) (:rcv prg))
)

(defn i-rcv [x prg]
    (->Program (:memory prg) (inc (:pc prg)) (:snd prg)
        (if (not= 0 (get-value x prg)) (:snd prg) nil))
)

(defn decode-1 [op]
    ; (println op)
    (let [sop (str/split op #" ")] ;"
        (case (sop 0)
            "snd" (partial i-snd (sop 1))
            "set" (partial i-set (sop 1) (sop 2))
            "add" (partial i-add (sop 1) (sop 2))
            "mul" (partial i-mul (sop 1) (sop 2))
            "mod" (partial i-mod (sop 1) (sop 2))
            "rcv" (partial i-rcv (sop 1))
            "jgz" (partial i-jgz (sop 1) (sop 2))
        )
    )
)

(defn i-snd-2 [x prg]
    (let [sc (inc (:snd-count (:memory prg) 0))]
        (->Program (conj (:memory prg) {:snd-count sc}) (inc (:pc prg)) (get-value x prg) (:rcv prg)))
)

(defn i-rcv-2 [x prg]
    (if (some? (peek (:rcv prg)))
        (->Program (conj (:memory prg) {:stalled false} {x (peek (:rcv prg))}) (inc (:pc prg)) (:snd prg) (pop (:rcv prg)))
        (->Program (conj (:memory prg) {:stalled true}) (:pc prg) (:snd prg) (:rcv prg))
    )
)

(defn decode-2 [op]
    ; (println op)
    (let [sop (str/split op #" ")] ;"
        (case (sop 0)
            "snd" (partial i-snd-2 (sop 1))
            "set" (partial i-set   (sop 1) (sop 2))
            "add" (partial i-add   (sop 1) (sop 2))
            "mul" (partial i-mul   (sop 1) (sop 2))
            "mod" (partial i-mod   (sop 1) (sop 2))
            "rcv" (partial i-rcv-2 (sop 1))
            "jgz" (partial i-jgz   (sop 1) (sop 2))
        )
    )
)

; phase 1
(defn run-until-output [program]
    (loop [prg (->Program {} 0 nil nil)]
        (if (some? (:rcv prg)) 
            (:rcv prg) 
            (recur ((decode-1 (program (:pc prg))) prg))))
)

(defn chk-idx [idx v] (and (>= idx 0) (< idx (count v))))

(defn transmit-msg [prg-1 prg-2]
    (let [msg-1 (:snd prg-1) msg-2 (:snd prg-2)]
        (defn push-q [msg q] (if (some? msg) (conj q msg) q))
        (if (or (some? msg-1) (some? msg-2))
            [
                (->Program (:memory prg-1) (:pc prg-1) nil (push-q msg-2 (:rcv prg-1)))
                (->Program (:memory prg-2) (:pc prg-2) nil (push-q msg-1 (:rcv prg-2)))
            ]
            [prg-1 prg-2]))
)

; phase 2
(defn run-duet [program]
    (loop [
        step 1
        prg-1 (->Program {"p" 0} 0 nil (queue))
        prg-2 (->Program {"p" 1} 0 nil (queue))
    ]
        (let [
            stopped (or 
                (and (not (chk-idx (:pc prg-1) program)) (not (chk-idx (:pc prg-2) program)))
                (and ((:memory prg-1) :stalled false) ((:memory prg-2) :stalled false)))
            [new-prg-1 new-prg-2]
                (transmit-msg 
                    (if (chk-idx (:pc prg-1) program) ((decode-2 (program (:pc prg-1))) prg-1) prg-1)
                    (if (chk-idx (:pc prg-2) program) ((decode-2 (program (:pc prg-2))) prg-2) prg-2))
            ]
            (if stopped
                ((:memory prg-2) :snd-count 0)
                (recur (inc step) new-prg-1 new-prg-2)
            )
        )
    )
)

(defn -main
    [& args]
    (def code (str/split (slurp "input.txt") #"\n"))
    (println "First recovered frequency:" (run-until-output code))
    (println "Duet: program 0 send count:" (run-duet code))
)


; (load-file "src/day_18/core.clj")
; 1: 3188
; 2: 7112

