(ns day-4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn count-full-overlaps
  [input]
  (reduce (fn count-ols
            [acc row]
            (let [[left-start left-end right-start right-end]
                  (->> (str/split row #",")
                       (map #(str/split % #"-"))
                       flatten
                       (map #(Integer/parseInt %)))
                  left-sections (-> (range left-start (inc left-end))
                                    set)
                  right-sections (-> (range right-start (inc right-end))
                                     set)
                  overlap (set/intersection left-sections right-sections)]
              (if (or (= overlap left-sections)
                      (= overlap right-sections))
                (inc acc)
                acc)))
          0
          input))

(defn count-overlaps
  [input]
  (reduce (fn count-ols
            [acc row]
            (let [[left-start left-end right-start right-end]
                  (->> (str/split row #",")
                       (map #(str/split % #"-"))
                       flatten
                       (map #(Integer/parseInt %)))
                  left-sections (-> (range left-start (inc left-end))
                                    set)
                  right-sections (-> (range right-start (inc right-end))
                                     set)
                  overlap (set/intersection left-sections right-sections)]
              (if (seq overlap)
                (inc acc)
                acc)))
          0
          input))

(comment
  (with-open [rdr (-> "day-4.txt"
                      io/resource
                      io/reader)]
    (-> rdr
        line-seq
        count-overlaps)))

