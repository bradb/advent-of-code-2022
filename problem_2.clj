(ns problem-2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [portal.api :as portal]))

(def ^:private choice->score {"X" 1, "Y" 2, "Z" 3})
(def ^:private choice->beats {"A" "Z", "B" "X", "C" "Y"})
(def ^:private left->right {"A" "X", "B" "Y", "C" "Z"})

(defn- ^:private draw?
  [left right]
  (= (left->right left) right))

(defn- beats?
  [x y]
  (= (choice->beats x) y))

(defn input->score
  [input]
  (with-open [rdr (io/reader input)]
    (reduce (fn add-row-score
              [total row]
              (let [[their-choice my-choice] (str/split row #"\s+" )
                    outcome-score (cond
                                    (beats? their-choice my-choice) 0
                                    (draw? their-choice my-choice) 3
                                    :else 6)
                    choice-score (choice->score my-choice)]
                (tap> outcome-score)
                (tap> choice-score)
                (+ total outcome-score choice-score)))
            0
            (line-seq rdr))))

(comment
  (let [p (portal/open)]
    (add-tap #'portal/submit)))
