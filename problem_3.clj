(ns problem-3
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

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

(defn sum-group-badges
  [rucksacks]
  (let [groups (partition-all 3 rucksacks)
        item->priority (fn [item]
                         (if (>= item 97)
                           (- item 96)
                           (- item 38)))]
    (reduce
     (fn sum-badges
       [acc group]
       (let [item (->> group
                       (map #(-> %
                                 (str/split #"")
                                 set))
                       (apply set/intersection)
                       first
                       (.getBytes)
                       (map int)
                       first
                       item->priority)]
         (+ acc item)))
     0
     groups)))

(comment
  (with-open [rdr (-> "problem-3-1.txt"
                      io/resource
                      io/reader)]
    (-> rdr
        line-seq
        sum-group-badges)))
