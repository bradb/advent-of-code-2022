(ns problem-1
  (:require [clojure.string :as str]))

(defn input->calorie-totals
  [input]
  (->> input
       slurp
       str/split-lines
       (partition-by str/blank?)
       (reduce (fn [acc [x & _ :as xs]]
                 (if (seq x)
                   (conj acc (apply + (map #(Integer/parseInt %) xs)))
                   acc))
               [])))

(defn max-cals
  [input]
  (->> input
       input->calorie-totals
       (apply max)))

(defn top-n-cals
  [input n]
  (let [sorted-cals (->> input
                         input->calorie-totals
                         sort
                         reverse)]
    (->> (take n sorted-cals)
         (apply +))))

(comment
  (let [input "data/problem-1-1.txt"]
    (top-n-cals input 3)))

