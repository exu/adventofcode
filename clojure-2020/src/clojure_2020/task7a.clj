(ns clojure-2020.core
  (:gen-class)
  (:require [clojure.string :as s]
            [clojure.test :refer [run-tests is are deftest]]))




(def ex "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")

(defn parse-element [el]
  (let [[_ count type color] (re-matches #"\s?(\d+) (\w+) (\w+) bags?\.?" el)]
    {:count count :type type :color color}))

(defn parse-head-element [el]
  (let [[_ type color] (re-matches #"(\w+) (\w+)" el)]
    {:type type :color color}))

(defn parse-line [line]
  (as-> (s/split line #" bags contain") data
    [(parse-head-element (first data)) (map parse-element (s/split (second data) #", "))]))

(parse-line "light red bags contain 1 bright white bag, 2 muted yellow bags.")

(s/split (second (s/split "light red bags contain 1 bright white bag, 2 muted yellow bags." #" bags contain")) #", ")



(defn result []
  (as-> (slurp "resources/task7.txt") d
    (s/split-lines d)))


(println (result))

(deftest result-test
  (is (= [{:type "light", :color "red"}, '({:count "1", :type "bright", :color "white"}, {:count "2", :type "muted", :color "yellow"})]
         (parse-line "light red bags contain 1 bright white bag, 2 muted yellow bags."))))




(deftest are-test
  (are [x y] (= x y)
    "x" "x"
    "y" "y"))
