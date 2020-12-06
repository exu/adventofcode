(ns clojure-2020.core
  (:gen-class)
  (:require [clojure.string :as s]
            [clojure.test :refer [run-tests is are deftest]]
            [clojure.spec.alpha :as spec]))


(defn calulate-group-occurrences [group]
  (as-> group data
    (for [e data] (zipmap e (repeat (count e) 1)))     ; {"y" 1, "m" 1, "w" 1}
    {:count (count data) :answers (reduce (fn [acc m] (merge-with + acc m)) data)}))

(defn filter-value-equals-count [m]
  (into {} (filter #(= (:count m) (second %)) (:answers m))))

(as-> (slurp "resources/task6.txt") data
  (s/split data #"\n\n")                    ; split groups
  (map #(s/split-lines %) data)             ; split lines in each groups
  (for [el data] (map #(s/split % #"") el)) ; split internal array elems
  (map calulate-group-occurrences data)
  (map filter-value-equals-count data)
  (map count data)
  (reduce + data)
  )






;; ------------------------------------------------------------------------------
;; playing with example data
;; ------------------------------------------------------------------------------
;; (def example
;;   "abc

;; a
;; b
;; c

;; ab
;; ac

;; a
;; a
;; a
;; a

;; b")

;; (defn remove-newlines [s] (remove #(= \newline %) s))


;; (def example-group '(["y" "m" "w"] ["w"] ["w" "m"] ["v" "s" "w"] ["w" "m"]))

;; (def maps (for [e example-group]
;;             (zipmap e (repeat (count e) 1))
;;             ))

;; (pprint maps)
;; (reduce (fn [acc m] (merge-with + acc m)) maps)


;; (as-> example-group data
;;   (for [e data] (zipmap e (repeat (count e) 1)))     ; {"y" 1, "m" 1, "w" 1}
;;   (reduce (fn [acc m] (merge-with + acc m)) data))   ; sum all maps together


;; ;; tests
;; (remove #(= \newline %) "dasjkdjksad\nasdsad\n")
;; (filter-value-equals-count {:count 5, :answers {"y" 1, "m" 3, "w" 5, "v" 1, "s" 1}})
