(ns day-5
  (:require [clojure.java.io :as io]
            [portal.api :as portal]
            [clojure.string :as str]))

(def ^:private stack-row-re #"\[")
(def ^:private stack-numbers-re #"^\s*1")
(def ^:private instruction-re #"\s*move")
(def ^:private parse-instruction-re #"move (\d+) from (\d+) to (\d+)")

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

        stacks (some->> (:stacks parsed)
                        (map #(partition 4 %))
                        reverse
                        (apply map (fn [& xs]
                                     (->> (for [x xs
                                                :let [value (str/join x)]
                                                :when (not= value "    ")]
                                            (second x))
                                          (apply vector))))
                        vec)

        instructions (map (fn instruction->map
                            [x]
                            (let [[_ move from to] (re-matches parse-instruction-re x)]
                              {:move (Integer/parseInt move), :from (Integer/parseInt from), :to (Integer/parseInt to)}))
                          (:instructions parsed))

        updated-stacks (reduce
                        (fn [acc {:keys [move from to] :as _instr}]
                          (let [from-idx (dec from)
                                to-idx (dec to)
                                from-stack (get acc from-idx)]
                            (-> acc
                                (update from-idx (fn [v]
                                                   (into [] (drop-last move v))))
                                (update to-idx (fn [v]
                                                 (into [] (concat v (-> move
                                                                     (take-last from-stack)
                                                                     reverse))))))))
                        stacks
                        instructions)]
    (tap> (->> updated-stacks
               (map peek)
               (str/join)))))

(comment
  (portal/open)
  (add-tap #'portal/submit)

  (let [test-data "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"]
    (with-open [rdr (-> "day-5.txt"
                        io/resource
                        io/reader)]
      (portal/clear)
      (-> test-data
          str/split-lines
          tops))))
