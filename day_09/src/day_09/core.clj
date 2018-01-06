(ns day-09.core
  (:gen-class))

(require '[clojure.string :as str])

(defrecord StateTrans [next-func chr chr-type])

(defn state-func-ignore [prev-f chr]
    (->StateTrans prev-f chr :ignore)
)

(defn state-func-in-garbage [chr])

(defn state-func-good [chr]
    (->StateTrans
        (case chr
            \! (fn [c] (state-func-ignore state-func-good c))
            \< state-func-in-garbage
            state-func-good
        )
        chr
        (case chr
            \! :ignore-start
            \< :garbage-start
            :good
        )
    )
)

(defn state-func-in-garbage [chr]
    (->StateTrans
        (case chr
            \! (fn [c] (state-func-ignore state-func-in-garbage c))
            \> state-func-good
            state-func-in-garbage
        )
        chr
        (case chr
            \! :ignore
            \> :garbage-end
            :garbage
        )
    )
)


(defn clean-stream [stream-seq]
    ; if ! -> ignore next char
    ; if < -> garbage starts
    ; if > -> garbage ends
    (let []
        (def state-func state-func-good)
        (def good-chars [])
        (def bad-chars [])
        (doseq [chr stream-seq]
            (def trans (state-func chr))
            (if (= :good (:chr-type trans)) (def good-chars (conj good-chars (:chr trans))))
            (if (= :garbage (:chr-type trans)) (def bad-chars (conj bad-chars (:chr trans))))
            (def state-func (:next-func trans))
        )
        [good-chars bad-chars]
    )
)

(defn count-groups [good-seq level]
    (case (first good-seq)
        \} (+ level (count-groups (rest good-seq) (dec level)))
        \{ (count-groups (rest good-seq) (inc level))
        0
    )
)


(defn -main
    [& args]
    (def stream (seq (str/trim (slurp "input.txt"))))
    (def streams (clean-stream stream))
    (println "Score of groups is"
        (count-groups (filter (fn [c] (not= c \,)) (streams 0)) 0))
    (println "Garbage count is" (count (streams 1)))
)
