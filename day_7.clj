(ns day-7
  (:require [clojure.string :as str]
            [portal.api :as portal]
            [clojure.java.io :as io]))

(def test-data "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(defn parse-input
  [input]
  (for [line input
        :let [[p1 p2 p3 :as parts] (str/split line #"\s+")]]
    (cond
      (= p1 "$")
      [(keyword p2) p3]

      (= p1 "dir")
      [(keyword p1) p2]

      (re-seq #"^\d+$" p1)
      [:file p2 (Integer/parseInt p1)]

      :else
      (throw (Exception. (str "don't know how to parse line: " line))))))

(comment
  (let [path ["a" "b" "c" "d"]
        itr (iterate pop path)]
    (take (count path) itr)))

(defn calc-sizes
  [inputs]
  (loop [[first-input & rest-inputs] inputs
         sizes {}
         path []]
    (if (seq first-input)
      (let [[arg1 arg2 arg3] first-input]
        (case arg1
          :cd
          (if (= arg2 "..")
            (recur rest-inputs sizes (pop path))
            (let [new-path (conj path arg2)]
              (recur rest-inputs (assoc sizes new-path 0) new-path)))

          :ls
          (recur rest-inputs sizes path)

          :dir
          (recur rest-inputs (assoc sizes (conj path arg2) 0) path)

          :file
          (let [file-size arg3
                updated-sizes (reduce
                               (fn update-sizes
                                 [acc path]
                                 (update acc path + file-size))
                               sizes
                               (->> path
                                    (iterate pop)
                                    (take (count path))))]
            (recur rest-inputs updated-sizes path))))
      sizes)))

(defn sizes->smallest-dir-to-delete
  [path->size total-disk-space space-required]
  (let [root-space (get path->size ["/"])
        unused-space (- total-disk-space root-space)
        space-to-free (- space-required unused-space)
        sizes (->> path->size
                   (map second)
                   sort)]
    (->> sizes
         (filter #(>= % space-to-free))
         first)))

(comment
  (portal/clear)
  (add-tap #'portal/submit)


  (let [inputs (parse-input (-> "day-7.txt"
                                io/resource
                                io/reader
                                line-seq))]
    (portal/clear)
    (tap> (->> inputs
               calc-sizes
               (map second)
               (filter #(<= % 100000 ))
               (apply +))))

  (let [inputs (parse-input (-> "day-7.txt"
                                io/resource
                                io/reader
                                line-seq))]
    (portal/clear)
    (sizes->smallest-dir-to-delete (calc-sizes inputs) 70000000 30000000)))




