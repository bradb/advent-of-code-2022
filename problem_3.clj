(ns problem-3
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

(defn sum-priorities
  [rucksacks]
  (reduce (fn sum-priorities
            [acc rucksack-contents]
            (let [[s1 s2]
                  (->> rucksack-contents
                       (split-at (/ (count rucksack-contents) 2))
                       (map set))

                  dupe-int (-> (set/intersection s1 s2)
                               first
                               int)]
              (if (>= dupe-int 97)
                (+ acc (- dupe-int 96))
                (+ acc (- dupe-int 38)))))
          0
          rucksacks))

(comment
  (with-open [rdr (-> "problem-3-1.txt"
                      io/resource
                      io/reader)]
    (sum-priorities (line-seq rdr))))
