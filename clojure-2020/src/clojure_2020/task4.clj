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

;; You can continue to ignore the cid field, but each other field has
;; strict rules about what values are valid for automatic validation:

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



;; PARSERS


(do

  (defn parse [in]
    (let [[_ key val] (re-matches #"([a-z]{3}):([a-z0-9#]+)" in)]
      [(keyword (str *ns*) key) val]))

  (defn parse-passport [in]
    (map parse in))

  (def height-pattern #"(\d+)(in|cm)")

  (defn parse-height [in]
    (let [[_ val unit] (re-matches height-pattern in)]
      [(read-string val) unit]))

  (defn get-height [in]
    (get (parse-height in) 0))


  ;; VALIDATION login
  (defn valid-cm? [in]
    (let [hgt (get-height in)]
      (and (re-matches #".*cm$" in) (>= hgt 150) (<= hgt 193))))

  (defn valid-in? [in]
    (let [hgt (get-height in)]
      (and (re-matches #".*in$" in) (>= hgt 59) (<= hgt 76))))

  (defn valid-color? [in]
    (re-matches #"^#[0-9a-f]{6}$" in))

  (defn valid-pid? [in]
    (re-matches #"^[0-9]{9}$" in))

  (defn not-valid? [key val]
    (not (valid? key val)))


  (defn passport-elements-valid? [passport]
    (let [keys (set (->> passport (map first)))]
      (or (= keys #{::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid ::cid})
          (= keys #{::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid})))
    )

  (passport-elements-valid? example-passport)

  ;; SPECS for validation purposes
  (spec/def ::byr (spec/and #(>= (read-string %) 1920) #(<= (read-string %) 2002)))
  (spec/def ::iyr (spec/and #(>= (read-string %) 2010) #(<= (read-string %) 2020)))
  (spec/def ::eyr (spec/and #(>= (read-string %) 2020) #(<= (read-string %) 2030)))
  (spec/def ::hgt (spec/and
                   #(re-matches height-pattern %)
                   (spec/or
                    :cm valid-cm?
                    :in valid-in?
                    )))
  (spec/def ::ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
  (spec/def ::hcl valid-color?)
  (spec/def ::pid valid-pid?)
  (spec/def ::cid any?)

  (spec/def ::passport passport-elements-valid?)

  (defn passport-valid? [passport]
    (let [
          groups (map valid? passport)
          ;; _ (println "GROUPS:" groups)
          groups-valid (every? true? groups)
          ;; _ (println "valid:" is-valid)
          spec-valid (spec/valid? ::passport passport)
          ]
      (and groups-valid spec-valid))
    )


  (defn valid?
    ([] false)
    ([kv] (spec/valid? (get kv 0) (get kv 1)))
    ([key val] (spec/valid? key val))
    )

  (def example-passport '([:clojure-2020.core/hcl "#fffffd"] [:clojure-2020.core/byr "1951"] [:clojure-2020.core/cid "321"] [:clojure-2020.core/iyr "2017"] [:clojure-2020.core/eyr "2022"] [:clojure-2020.core/ecl "brn"] [:clojure-2020.core/hgt "62in"] [:clojure-2020.core/pid "#6ef4e1"]))
  (passport-valid? example-passport)


  (map println (range 10))
  ;; now mix everyting
  (as-> (slurp "resources/task4.txt") pass
    (s/split pass #"\n\n")
    ;; (take 3 pass)
    (map #(s/split %1 #" |\n") pass)
    (map parse-passport pass)
    (map passport-valid? pass)
    (frequencies pass)
    ;; (map println pass)
    ;; (count pass)

    )

  )



;; TESTING

;; TDD for the rescue :)
(do
  (deftest passports
    (deftest parse-test
      (is (= [::cid "280"] (parse "cid:280")))
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
    (deftest eyr-test
      (is (valid? ::eyr "2020"))
      (is (valid? ::eyr "2030"))
      (is (valid? ::eyr "2021"))
      (is (valid? ::eyr "2029"))
      (is (not-valid? ::eyr "2019"))
      (is (not-valid? ::eyr "2031"))
      )
    (deftest hgt-test
      (is (valid? ::hgt "150cm"))
      (is (not-valid? ::hgt "10cm"))
      (is (not-valid? ::hgt "1099cm"))
      (is (valid? ::hgt "60in"))
      (is (not-valid? ::hgt "90in"))
      (is (not-valid? ::hgt "10in"))
      )
    (deftest hcl-test
      (is (valid? ::hcl "#fefefe"))
      (is (valid? ::hcl "#dead01"))
      (is (not-valid? ::hcl "green"))
      (is (not-valid? ::hcl "pinky"))
      (is (not-valid? ::hcl "#fefefe00"))
      )
    (deftest pid-test
      (is (valid? ::pid "139209129"))
      (is (valid? ::pid "000000009"))
      (is (not-valid? ::pid "12abcdef0"))
      (is (not-valid? ::pid "000 000 000"))
      (is (not-valid? ::pid "aaaa"))
      (is (not-valid? ::pid " 000000001 "))
      )
    )
  (run-tests))




(spec/explain ::hgt "150cm")
(spec/valid? ::hgt "150cm")


(as-> [[1 2 3 4 5] [1]] i
(first i)
(map #(* % %) i))


(keys {::cid "280"})
