;; https://adventofcode.com/2020/day/3
(ns clojure-2020.core
  (:gen-class)
  (:require [clojure.string :as s]))



(defn read-file [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (->> rdr
         line-seq
         vec
         )))

(let [
      tree? #(= \# %1)
      lines (read-file "resources/task3.txt")
      width (count (nth lines 0))
      positions  (iterate #(mod (+ 3 %1) width) 0)    ;;  jumps by 3 for each row
      spots (map nth lines positions)
      trees (filter tree? spots)
      trees-count (count trees)
      free (filter #(not (tree? %)) spots)
      free-count (count free)
      ]

  [trees-count free-count]
  )


;; Determine the number of trees you would encounter if, for each of
;; the following slopes, you start at the top-left corner and traverse
;; the map all the way to the bottom:
(def lines (read-file "resources/task3.txt"))

(defn count-trees [lines steps-right]
  (let [
        tree?       #(= \# %1)
        width       (count (nth lines 0))
        positions   (iterate #(mod (+ steps-right %1) width) 0) ;;  jumps by 3 for each row
        spots       (map nth lines positions)
        trees       (filter tree? spots)
        trees-count (count trees)
        ]

    trees-count))

(*(count-trees lines 1)
(count-trees lines 3)
(count-trees lines 5)
(count-trees lines 7)
(count-trees (take-nth 2 lines) 1))

;; Right 1, down 1.
;; Right 3, down 1. (This is the slope you already checked.)
;; Right 5, down 1.
;; Right 7, down 1.
;; Right 1, down 2.




;; map works with multiple collections maps each elem from first and second coll etc
;; nth "dupa" 1
;; nth "lampa" 2
;; nth "pampa" 3
(map nth
     ["dupa", "lampa", "pampa"]
     [1 2 3])
