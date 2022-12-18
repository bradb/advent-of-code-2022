(ns day-7
  (:require [clojure.string :as str]
            [portal.api :as portal]))

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

(defn sum-dir-sizes
  [input max-dir-size]
  (for [line input
        :let [[p1 p2 p3 :as parts] (str/split line #"\s+")]]
    (cond
      (= p1 "$")
      [(keyword p2) p3]

      (= p1 "dir")
      [(keyword p1) p2]

      (re-seq #"^\d+$" p1)
      [:file p2 p1]

      :else
      (throw (Exception. (str "don't know how to parse line: " line))))))

(comment
  (let []
    (portal/clear)
    (tap> (-> test-data
              str/split-lines
              (sum-dir-sizes 100000)))))
