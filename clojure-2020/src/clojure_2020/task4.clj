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



;; part 2
;; Moar rulz

;; You can continue to ignore the cid field, but each other field has strict rules about what values are valid for automatic validation:

;; - byr (Birth Year) - four digits; at least 1920 and at most 2002.
;; - iyr (Issue Year) - four digits; at least 2010 and at most 2020.
;; - eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
;; - hgt (Height) - a number followed by either cm or in:
;; - If cm, the number must be at least 150 and at most 193.
;; - If in, the number must be at least 59 and at most 76.
;; - hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
;; - ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
;; - pid (Passport ID) - a nine-digit number, including leading zeroes.
;; - cid (Country ID) - ignored, missing or not.

(defn parse [in]
  (let [[_ key val] (re-matches #"([a-z]{3}):([a-z0-9#]+)" in)]
    #{(keyword (str *ns*) key) val}))

(def height-pattern #"(\d+)(in|cm)")

(defn parse-height [in]
  (let [[_ val unit] (re-matches height-pattern in)]
    [(read-string val) unit]))

(defn get-height [in]
  (get (parse-height in) 0))

(defn is-cm? [in] (re-matches #".*cm$" in))

(defn valid-cm? [in]
  (let [hgt (get-height in)]
    (and (>= hgt 150) (<= hgt 193))))

(defn is-in? [in] (re-matches #".*in$" in))

(defn valid-in? [in]
  (let [hgt (get-height in)]
  (and (>= hgt 59) (<= hgt 76))))

(spec/def ::ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
(spec/def ::byr (spec/and #(>= (read-string %) 1920) #(<= (read-string %) 2002)))
(spec/def ::iyr (spec/and #(>= (read-string %) 2010) #(<= (read-string %) 2020)))
(spec/def ::hgt (spec/and
                 #(re-matches height-pattern %)
                 (spec/or
                  :cm (spec/and is-cm? valid-cm?)
                  :in (spec/and is-in? valid-in?)
                  )))

(spec/explain ::hgt "150cm")


(spec/valid? ::hgt "150cm")


(defn valid? [key val]
  (spec/valid? key val))

(defn not-valid? [key val]
  (not (valid? key val)))


;; TDD for the rescue :)
(do
  (deftest passports
    (deftest parse-test
      (is (= #{::cid "280"} (parse "cid:280")))
      )
    (deftest parse-height-test
      (is (= [123 "cm"] (parse-height "123cm")))
      (is (= [3289 "in"] (parse-height "3289in")))
      )
    (deftest ecl-test
      (is (= true (valid? ::ecl "gry")))
      (is (= false (valid? ::ecl "boooo")))
      )
    (deftest byr-test
      (is (valid? ::byr "1920"))
      (is (valid? ::byr "1933"))
      (is (valid? ::byr "2000"))
      (is (valid? ::byr "2002"))
      (is (not-valid? ::byr "1919"))
      (is (not-valid? ::byr "2010"))
      )
    (deftest iyr-test
      (is (valid? ::iyr "2010"))
      (is (valid? ::iyr "2011"))
      (is (valid? ::iyr "2019"))
      (is (valid? ::iyr "2020"))
      (is (not-valid? ::iyr "2009"))
      (is (not-valid? ::iyr "2021"))
      (is (not-valid? ::iyr "1233"))
      )
    (deftest hgt-test
      (is (valid? ::hgt "150cm"))
      (is (not-valid? ::hgt "10cm"))
      (is (not-valid? ::hgt "1099cm"))
      (is (valid? ::hgt "60in"))
      (is (not-valid? ::hgt "90in"))
      (is (not-valid? ::hgt "10in"))
      )

    )
  (run-tests))



;; testing
(as-> [[1 2 3 4 5] [1]] i
(first i)
(map #(* % %) i))
