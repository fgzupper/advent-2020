(ns advent-2020.day-07
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))
;; Keeping most comments.
;; I'll let someone else do the u30 line Clojure thing.

(def example
  (str/split-lines
    "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags."))

(def data
  (-> (io/resource "day_07_input") slurp
      (str/split-lines)
      ))

;; Parsing for meaning, I see.
;; x " contain " <number> y.
;; (or , <number> z, ...)
;; So, a graph.
;; How do we want to persist it?
;; Mmm, number will be important eventually.

(defn parse-relation [r]
  (let [[n & wds] (str/split r #"\s")
        tkn (str/join "_" wds)]
    {(keyword (if (str/ends-with? tkn "s")
                tkn
                (str tkn "s")))
     (edn/read-string n)}))
;; The single/plural shenanigan cost me a lot of time to catch.
#_(parse-relation "5 shiny gold bags")
#_(parse-relation "1 shiny gold bag")

(defn parse-line [line]
  (let [[root relations] (str/split line #" contain ")
        root-kw (-> root (str/replace #"\s" "_") keyword)
        rlns (-> relations
                 (str/replace #"\." "")
                 (str/split #", "))
        nodes (map parse-relation rlns)]
    {root-kw (reduce merge nodes)}))

(def eg
  (->> example
       (map parse-line)
       (reduce merge)))

(def graph
  (->> data
       (map parse-line)
       (reduce merge)))

;; So...
;; If it has gold, add to special.
;; Else, recurse with all the ones it has.
;; [so get all next steps]
(defn path-to-gold [special visited graph nxt]
  (let [[kw m] nxt
        tgts (-> m keys set)]
    (when (and kw (not (@visited kw)))
      (swap! visited conj kw)
      (when (or (:shiny_gold_bags m)
                (seq (set/intersection @special tgts))
                (some (partial path-to-gold special visited graph)
                      (map (fn [x] [x (graph x)]) tgts)))
        (swap! special conj kw)))))
;; (I had ifs and explicit true/false returns on my first pass.
;;  Now doing when that return nil if false, and swap! which
;;  returns a set (truthy). TBH, first way was clearer.)

(defn num-of-gold-holdings [graph]
  (let [special (atom #{})
        visited (atom #{})]
    (run! (partial path-to-gold special visited graph) graph)
    (count @special)))

;; New problem:
;; how many bags inside shiny gold bag?
;; (kinda same thing, but count bags, starting at gold)

(def ex2
  (str/split-lines
    "shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags."))
(def eg2
  (->> ex2
       (map parse-line)
       (reduce merge)))

;; So, things to do.
;; look up number in cache
;; if not there, look up children
(def cache (atom {})) ; being lazy/evil and not let-scoping this.
(defn bags-within [graph bag-kw]
  (if (@cache bag-kw)
    (@cache bag-kw)
    (let [mp (bag-kw graph)
          cnt (->> mp
           (map (fn [[k n]]
                  (if (and (number? n) (pos? n))
                    (* n (inc (bags-within graph k)))
                    0)))
           (reduce +))]
      (swap! cache assoc bag-kw cnt)
      cnt)))

(defn -main []
  (time
    (do
      (println "Example for #1")
      (println (num-of-gold-holdings eg))
      (println "Real for #1")
      (println (num-of-gold-holdings graph))
      (reset! cache {}) ; you pay for evil
      (println "Example for #2")
      (println (bags-within eg2 :shiny_gold_bags))
      (reset! cache {}) ; but occasionally still put up with it.
      (println "Real for #2")
      (println (bags-within graph :shiny_gold_bags)))))
