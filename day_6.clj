(ns day-6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn stream->start-of-packet
  [s]
  (let [s (vec s)]
    (reduce
     (fn [_ i]
       (let [sv (subvec s (- i 3) (inc i))]
         (when (= 4 (count (set sv)))
           (reduced (inc i)))))
     (range 3 (count s)))))

(comment
  (let [s (-> "day-6.txt"
              io/resource
              slurp
              str/trim)]
    (stream->start-of-packet s)))

(comment
  (stream->start-of-packet "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")
  (stream->start-of-packet "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
  (stream->start-of-packet "bvwbjplbgvbhsrlpgdmjqwftvncz"))
