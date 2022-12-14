(ns problem-1
  (:require [clojure.string :as str]))

(defn max-cals
  []
  (->> "data/problem-1-1.txt"
      slurp
      str/split-lines
      (partition-by str/blank?)
      (reduce (fn [acc [x & _ :as xs]]
                (if (seq x)
                  (conj acc (apply + (map #(Integer/parseInt %) xs)))
                  acc))
              [])
      (apply max)))

(comment
  (max-cals))

