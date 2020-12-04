;; https://adventofcode.com/2020/day/2
(ns clojure-2020.core
  (:gen-class)
  (:require [clojure.string :as s]))

(def valid "2-8 d: pddzddkdvqgxndd")
(def invalid "1-3 b: cdebbnnnndsnmbbbbfg")
(def filename "resources/task2.txt")

(defn isvalid [line]
  (let [
        [_ from to letter input] (re-matches #"(\d+)-(\d+) (\w): (\w+)" line) ; destructure line
        freq                     (frequencies (s/split input #""))            ; letters to array and calculate frequencies
        count                    (freq letter 0)                              ; get freq for given letter or 0 if nil
        ]
    (and (>= count (read-string from)) (<= count (read-string to))))) ; check conditions

(isvalid valid)
(isvalid invalid)

;; ok now read lines in file and calculate valid pass

(->> (slurp filename)
     s/split-lines
     (map isvalid)
     (filter true?)
     count)

;; or as stream

(with-open [rdr (clojure.java.io/reader filename)]
  (->> rdr
       line-seq
       (map isvalid)
       (filter true?)
       count
       ))


;; part 2 :)

;; 1-3 a: abcde is valid: position 1 contains a and position 3 does not.
;; 1-3 b: cdefg is invalid: neither position 1 nor position 3 contains b.
;; 2-9 c: ccccccccc is invalid: both position 2 and position 9 contain c.
(def valid2 "1-3 a: abcde")
(def invalid2a "1-3 b: cdefg")
(def invalid2b "2-9 c: ccccccccc")

(defn isvalid2 [line]
  (let [
        ;; line                         "2-9 c: ccccccccc"
        [_ first second letter input] (re-matches #"(\d+)-(\d+) (\w): (\w+)" line) ; destructure line
        first-pos                     (dec (read-string first))
        second-pos                    (dec (read-string second))
        letters                       (s/split input #"")
        first-letter                  (get letters first-pos "")                   ; letters to array and calculate frequencies
        second-letter                 (get letters second-pos "")                  ; letters to array and calculate frequencies
        ]
    (and (not (= first-letter second-letter))
         (or (= letter first-letter) (= letter second-letter)))
    ))

(isvalid2 valid2)
(isvalid2 invalid2a)
(isvalid2 invalid2b)

(with-open [rdr (clojure.java.io/reader filename)]
  (->> rdr
       line-seq
       (map isvalid2)
       (filter true?)
       count
       ))
