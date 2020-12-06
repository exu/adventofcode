;; https://adventofcode.com/2020/day/4
(ns clojure-2020.core
  (:gen-class)
  (:require [clojure.string :as s]
            [clojure.test :refer [run-tests is deftest]]
            [clojure.spec.alpha :as spec]))


;; The first passport is valid - all eight fields are present. The second
;; passport is invalid - it is missing hgt (the Height field).

;; The third passport is interesting; the only missing field is cid, so
;; it looks like data from North Pole Credentials, not a passport at all!
;; Surely, nobody would mind if you made the system temporarily ignore
;; missing cid fields. Treat this "passport" as valid.

;; The fourth passport is missing two fields, cid and byr. Missing cid is
;; fine, but missing any other field is not, so this passport is invalid.

(def example "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")




(defn valid? [passport]
  (or
   (= 8 (count passport))
   (and (= 7 (count passport)) (not-any? #{"cid"} passport))))

;; standard
(let [
      passports (s/split (slurp "resources/task4.txt") #"\n\n")
      fields    (map #(s/split %1 #" |\n") passports)
      keys      (for [f fields] (mapv #(subs %1 0 3) f))
      valid     (filter valid? keys)]

  (count valid))




;; as threading macro
(as-> (slurp "resources/task4.txt") pass
  (s/split pass #"\n\n")
  (map #(s/split %1 #" |\n") pass)
  (for [f pass] (mapv #(subs %1 0 3) f))
  (filter valid? pass)
  (count pass))
