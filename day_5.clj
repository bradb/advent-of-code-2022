(ns day-5
  (:require [clojure.java.io :as io]
            [portal.api :as portal]
            [clojure.string :as str]))

(def ^:private stack-row-re #"\[")
(def ^:private stack-numbers-re #"^\s*1")
(def ^:private instruction-re #"\s*move")

(defn tops
  [input]
  (let [parsed
        (reduce (fn parse
                  [acc row]
                  (cond
                    (re-seq stack-row-re row)
                    (update acc :stacks conj row)

                    (re-seq stack-numbers-re row)
                    (update acc :stack-numbers conj row)

                    (re-seq instruction-re row)
                    (update acc :instructions conj row)

                    :else
                    acc))
                {:stacks [], :stack-numbers [], :instructions []}
                input)

        num-stacks (some-> (:stack-numbers parsed)
                           first
                           str/trim
                           (str/split #"\s+")
                           peek
                           (Integer/parseInt))

        stacks (some-> num-stacks
                       (repeat [])
                       vec)

        stack-rows (some->> (:stacks parsed)
                            reverse
                            (map #(partition 4 %)))

        #_#_stacks (reduce (fn push-to-stack
                         [acc row]
                         (let [segments (partition 4 row)]
                           (reduce (fn conj-to-stack
                                     [acc' [first-char value _ _]]
                                     (case first-char
                                       \space
                                       acc

                                       \[
                                       kkkkkkkk))

                                   acc
                                   segments)))
                       stacks
                       stack-rows)]
    (tap> stack-rows)
    (tap> parsed)))

(comment
  (partition 4 "            [J] [Z] [G]            ")
  (-> 9
      (repeat [])
      vec)
  (portal/open)
  (add-tap #'portal/submit)

  (with-open [rdr (-> "day-5.txt"
                      io/resource
                      io/reader)]
    (portal/clear)
    (-> rdr
        line-seq
        tops)))
