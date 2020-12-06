(ns clojure-2020.core
  (:gen-class)
  (:require [clojure.string :as s]
            [clojure.test :refer [run-tests is are deftest]]
            [clojure.spec.alpha :as spec]))


;; You write a quick program to use your phone's camera to scan all of
;; the nearby boarding passes (your puzzle input); perhaps you can
;; find your seat through process of elimination.

;; Instead of zones or groups, this airline uses binary space
;; partitioning to seat people. A seat might be specified like
;; FBFBBFFRLR, where F means "front", B means "back", L means "left",
;; and R means "right".

;; The first 7 characters will either be F or B; these specify exactly
;; one of the 128 rows on the plane (numbered 0 through 127). Each
;; letter tells you which half of a region the given seat is in. Start
;; with the whole list of rows; the first letter indicates whether the
;; seat is in the front (0 through 63) or the back (64 through
;; 127). The next letter indicates which half of that region the seat
;; is in, and so on until you're left with exactly one row.

;; For example, consider just the first seven characters of FBFBBFFRLR:

;; Start by considering the whole range, rows 0 through 127.
;; F means to take the lower half, keeping rows 0 through 63.
;; B means to take the upper half, keeping rows 32 through 63.
;; F means to take the lower half, keeping rows 32 through 47.
;; B means to take the upper half, keeping rows 40 through 47.
;; B keeps rows 44 through 47.
;; F keeps rows 44 through 45.
;; The final F keeps the lower of the two, row 44.

;; The last three characters will be either L or R; these specify
;; exactly one of the 8 columns of seats on the plane (numbered 0
;; through 7). The same process as above proceeds again, this time
;; with only three steps. L means to keep the lower half, while R
;; means to keep the upper half.

;; For example, consider just the last 3 characters of FBFBBFFRLR:

;; Start by considering the whole range, columns 0 through 7.
;; R means to take the upper half, keeping columns 4 through 7.
;; L means to take the lower half, keeping columns 4 through 5.
;; The final R keeps the upper of the two, column 5.
;; So, decoding FBFBBFFRLR reveals that it is the seat at row 44, column 5.

;; Every seat also has a unique seat ID: multiply the row by 8, then
;; add the column. In this example, the seat has ID 44 * 8 + 5 = 357.

;; Here are some other boarding passes:

;; BFFFBBFRRR: row 70, column 7, seat ID 567.
;; FFFBBBFRRR: row 14, column 7, seat ID 119.
;; BBFFBBFRLL: row 102, column 4, seat ID 820.

;; As a sanity check, look through your list of boarding passes. What
;; is the highest seat ID on a boarding pass?

(do
  (defn split
    ([directions]
     (split (range 128) (range 8) directions))
    ([rows cols directions]
     (let [
           rows-split (split-at (/ (count rows) 2) rows)
           cols-split (split-at (/ (count cols) 2) cols)
           dir (first directions)

           ;; _ (println "Input cols" cols-split)
           ;; _ (println "Input rows" rows-split)
           ;; _ (println "Direction" dir)

           rows-half (cond
                       (= dir :F) (get rows-split 0)
                       (= dir :B) (get rows-split 1)
                       :else rows
                       )

           cols-half (cond
                       (= dir :L) (get cols-split 0)
                       (= dir :R) (get cols-split 1)
                       :else cols
                       )

           ;; _ (println "Rows half " rows-half)
           ;; _ (println "Cols half" cols-half)
           ;; _ (println )
           ;; _ (println )
           ;; _ (Thread/sleep 1000)
           ]

       (if (> (count directions) 1)
         (recur rows-half cols-half (rest directions))
         {:row (first rows-half) :col (first cols-half)}
         ))))

  (split [:B :F :F :F :B :B :F :R :R :R])

  ;; Every seat also has a unique seat ID: multiply the row by 8, then
  ;; add the column. In this example, the seat has ID 44 * 8 + 5 = 357.
  (defn calculate-id [in]
    (+ (* 8 (in :row)) (in :col)))

  (defn parse [in]
    (as-> in i
      (s/split i #"")
      (map keyword i) ))


  (def seats (->> (slurp "resources/task5.txt")
       (s/split-lines)
       (map parse)
       (map split)
       (map #(assoc % :id (calculate-id %)))
       ))

   (def row-with-missing-seat (->> seats
       (group-by :row)
       (map second)
       (map (fn [n] {:row ((get n 0) :row) :count (count n)} ))
       (filter #(< (% :count) 8))
       first
       :row
       ))

   (->> seats
        (filter #(= (% :row) row-with-missing-seat))
        (sort-by :col)
        (pprint)
        )

   ;; 629 :) is missing one





  (run-tests))
