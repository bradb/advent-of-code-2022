(ns day-5
  (:require [clojure.java.io :as io]
            [portal.api :as portal]
            [clojure.string :as str]))

(def ^:private stack-row-re #"\[")
(def ^:private stack-numbers-re #"^\s*1")
(def ^:private instruction-re #"\s*move")
(def ^:private parse-instruction-re #"move (\d+) from (\d+) to (\d+)")

(defn- parse-input
  [input]
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
          input))

(defn- parsed->stacks
  [parsed]
  (some->> (:stacks parsed)
           (map #(partition-all 4 %))
           reverse
           (apply map (fn [& xs]
                        (->> (for [x xs
                                   :let [value (str/join x)]
                                   :when (-> value
                                             str/trim
                                             seq)]
                               (second x))
                             (apply vector))))
           vec))

(defn- parsed->instructions
  [parsed]
  (map (fn instruction->map
         [x]
         (let [[_ move from to] (re-matches parse-instruction-re x)]
           {:move (Integer/parseInt move), :from (Integer/parseInt from), :to (Integer/parseInt to)}))
       (:instructions parsed)))

(defn- apply-instruction-9000
  [acc {:keys [move from to] :as _instr}]
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
(defn- apply-instruction-9001
  [acc {:keys [move from to] :as _instr}]
  (let [from-idx (dec from)
        to-idx (dec to)
        from-stack (get acc from-idx)]
    (-> acc
        (update from-idx (fn [v]
                           (into [] (drop-last move v))))
        (update to-idx (fn [v]
                         (into [] (concat v (take-last move from-stack))))))))

;; obviously could have further DRYed up this code, but keen to
;; move onto next problem now :)
(defn tops-9000
  [parsed]
  (let [stacks (parsed->stacks parsed)
        instructions (parsed->instructions parsed)

        updated-stacks (reduce
                        apply-instruction-9000
                        stacks
                        instructions)]
    (->> updated-stacks
         (map peek)
         (str/join))))

(defn tops-9001
  [parsed]
  (let [stacks (parsed->stacks parsed)
        instructions (parsed->instructions parsed)

        updated-stacks (reduce
                        apply-instruction-9001
                        stacks
                        instructions)]
    (->> updated-stacks
         (map peek)
         (str/join))))

(comment
  (portal/open)
  (add-tap #'portal/submit)

  (with-open [rdr (-> "day-5.txt"
                      io/resource
                      io/reader)]
    (portal/clear)
    (-> rdr
        line-seq
        parse-input
        tops-9001)))
