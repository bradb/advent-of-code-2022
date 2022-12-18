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

        stacks (some->> (:stacks parsed)
                        (map #(partition 4 %))
                        reverse
                        (apply map (fn [& xs]
                                     (->> (for [x xs
                                                :let [value (str/join x)]
                                                :when (not= value "    ")]
                                            (second x))
                                          (apply vector)))))]
    (tap> stacks)))

(comment
  (portal/open)
  (add-tap #'portal/submit)

  (with-open [rdr (-> "day-5.txt"
                      io/resource
                      io/reader)]
    (portal/clear)
    (-> rdr
        line-seq
        tops)))
