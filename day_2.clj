(ns day-2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:private right->score {"X" 1, "Y" 2, "Z" 3})
(def ^:private left->beats {"A" "Z", "B" "X", "C" "Y"})
(def ^:private left->right {"A" "X", "B" "Y", "C" "Z"})
(def ^:private desired-result {"X" :lose, "Y" :draw, "Z" :win})
(def ^:private left->loses-to {"A" "Y" "B" "Z" "C" "X"})

(defn- draw?
  [left right]
  (= (left->right left) right))

(defn- beats?
  [left right]
  (= (left->beats left) right))

(defn- desired-result->choice
  [result left]
  (case (desired-result result)
    :win
    (left->loses-to left)

    :draw
    (left->right left)

    :lose
    (left->beats left)))

(defn- add-row-score
  [total [their-choice my-choice :as _row]]
  (let [outcome-score (cond
                        (beats? their-choice my-choice) 0
                        (draw? their-choice my-choice) 3
                        :else 6)
        choice-score (right->score my-choice)]
    (+ total outcome-score choice-score)))

(defn input->score1
  [input]
  (with-open [rdr (-> input
                      io/resource
                      io/reader)]
    (reduce add-row-score 0 (->> rdr
                                 line-seq
                                 (map #(str/split % #"\s+"))))))

(defn input->score2
  [input]
  (with-open [rdr (-> input
                      io/resource
                      io/reader)]
    (reduce add-row-score 0 (->> rdr
                                 line-seq
                                 (map #(str/split % #"\s+"))
                                 (map (fn [[left right]]
                                        [left (desired-result->choice right left)]))))))

(comment
  (input->score1 "day-2.txt")
  (input->score2 "day-2.txt"))
