(ns day-6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn stream->marker
  [s n]
  (let [s (vec s)]
    (reduce
     (fn [_ i]
       (let [sv (subvec s (- i (dec n)) (inc i))]
         (when (= n (count (set sv)))
           (reduced (inc i)))))
     (range (dec n) (count s)))))

(comment
  (let [s (-> "day-6.txt"
              io/resource
              slurp
              str/trim)]
    (stream->marker s 14)))

(comment
  (stream->marker "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 4)
  (stream->marker "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 4)
  (stream->marker "bvwbjplbgvbhsrlpgdmjqwftvncz" 4)

  (stream->marker "mjqjpqmgbljsphdztnvjfqwrcgsmlb" 14)
  )
