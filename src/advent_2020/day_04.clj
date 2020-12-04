(ns advent-2020.day-04
  (:require [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))
;; This is literally the worst code I've written all year,
;; and I'm truly sorry.
;; It's not an accurate representation of the true beauty and power of Clojure.
;; It _IS_ an accurate representation of what happens when you have a dev
;; working as fast as possible, without any refactoring, to meet a deadline.
;; (Basically, never do that.)
;; I'll probably have to clean this up later to keep sane.
;; Again, so sorry.

(def data
  (-> (io/resource "day_04_input")
       slurp (str/split #"\n\n")
       ))

(def required-tokens
  #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"})

(defn valid? [pp]
  (let [tokens (str/split pp #"\s")
        fields (->> tokens 
                    (keep #(-> % (str/split #":") first))
                    set)
        ist (set/intersection required-tokens fields)]
    (= (count ist) (count required-tokens))))

(defn- height? [h]
  (or (->> (when (and (string? h)(re-matches #"\d{3}cm" h)) ["150cm" h "193cm"])
           sort
           ((juxt first last))
           (= ["150cm" "193cm"]))
      (->> (when (and (string? h)(re-matches #"\d{2}in" h))["59in" h "76in"])
           sort
           ((juxt first last))
           (= ["59in" "76in"]))))

;; A collection of predicates to check.
;; comp composes functions (right to left, or bottom to top)
;; So basically, the first one pulls out the value for :byr,
;; then does some nasty string comparisons to see if it is in a range.
;; (never do that... parse your numbers)
(def predicates
  [(comp (partial = ["1920" "2002"])
         (juxt first last)
         #(sort ["1920" % "2002"])
         :byr)
   (comp (partial = ["2010" "2020"])
         (juxt first last)
         #(sort ["2010" % "2020"])
         :iyr)
   (comp (partial = ["2020" "2030"])
         (juxt first last)
         #(sort ["2020" % "2030"])
         :eyr)
   (comp height? :hgt)
   (comp #(and (string? %)
               (re-matches #"#[0-9a-f]{6}" %)) :hcl)
   (comp #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} :ecl)
   (comp #(and (string? %) (re-matches #"\d{9}" %)) :pid)
   ])

(defn valid2? [pp]
  (let [tokens (str/split pp #"\s")
        ;; Takes all the tokens (words in a passport),
        ;; and throws them into a map of key-value pairs
        ;; Example: {:byr "2001" :eyr "2021" ...}
        fields (->> tokens 
                    (keep #(-> % (str/split #":") ((fn [[f s]] [(keyword f) s]))) )
                    (into {}))]
    ;; Once we have that map, we can run all of our predicates against it,
    ;; and make sure every one returns something truthy.
    (->> 
         (map (fn [f m] (f m)) predicates (repeat fields))
         (every? identity))))

(defn -main []
  (time
    (do 
      (println (count (filter valid? data)))
      (println (count (filter valid2? data)))
      )))
