;; https://adventofcode.com/2020/day/1
(ns clojure-2020.core
  (:gen-class)
  (:require [clojure.string :as s]))


;; first try "na pałę" but for those 2 nums there is almost no difference


(defn read1 [filename]
  (->> filename
       slurp
       s/split-lines
       (map read-string)))

(let [numbers (read1 "resources/task1.txt")]
  (for [a     numbers,
        b     numbers
        :when (> a b)
        :when (= (+ a b) 2020)]
    [a b]))


;; second try


(defn read2 [filename]
  (->> filename
       slurp
       s/split-lines
       (map read-string)
       set))
                                        ; set for contains?


(let [numbers (read2 "resources/task1.txt")]
  (for [a     numbers,       ; take all nums
        :let  [b (- 2020 a)] ; calculate one that missing
        :when (> a b)        ; limit second array
        :when (contains? numbers b)]          ; read1 generates lazyseq and we need standard one to search
    [a b]))


;; third fix reading for


(defn read3 [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (->> rdr
         line-seq
         (map read-string)
         set ; must be `set` or `doall` to convert from lazyseq, or need to pass code inside this threading macro we need all data here
         )))

(let [numbers (read2 "resources/task1.txt")]
  (for [a     numbers,       ; take all nums
        :let  [b (- 2020 a)] ; calculate one that missing
        :when (> a b)        ; limit second array
        :when (contains? numbers b)]          ; read1 generates lazyseq and we need standard one to search
    (* a b)))


;; part TWO three numbers


(let [numbers (read3 "resources/task1.txt")]
  (for [a     numbers,
        b     numbers
        c     numbers
        :when (> a b c)
        :when (= 2020 (+ a b c))]          ; read1 generates lazyseq and we need standard one to search
    (* a b c)))

(let [numbers (read3 "resources/task1.txt")]
  (for [a     numbers,
        b     numbers
        :let  [c (- 2020 a b)] ; 2020 minus combinations of all gives you third number
        :when (contains? numbers c)
        :when (> a b c)] ; search in numbers
    (* a b c)))
