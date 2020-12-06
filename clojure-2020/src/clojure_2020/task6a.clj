(ns clojure-2020.core
  (:gen-class)
  (:require [clojure.string :as s]
            [clojure.test :refer [run-tests is are deftest]]
            [clojure.spec.alpha :as spec]))

(as-> (slurp "resources/task6.txt") data
  (s/split data #"\n\n")
  (map (fn [group] (remove #(= \newline %) group)) data)
  (map #(into #{} %) data)
  (map count data)
  (reduce + data))
