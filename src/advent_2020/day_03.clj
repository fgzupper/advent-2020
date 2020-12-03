(ns advent-2020.day-03
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

(def data
  (->> (io/resource "day_03_input")
       slurp str/split-lines))

(def example
  ["..##......."
   "#...#...#.."
   ".#....#..#."
   "..#.#...#.#"
   ".#...##..#."
   "..#.##....."
   ".#.#.#....#"
   ".#........#"
   "#.##...#..."
   "#...##....#"
   ".#..#...#.#"])

;; So, for the tricks:
;; - Each row is a string; use `get` to grab a char from an index
;; - Instead of repeating a row, just mod the index by length of the row
;; - A fractional xdif (slope) can be used to skip a row (`zero? check skips).
(defn trees-hit-slope [field xdif]
  (reduce (fn [{:keys [idx hits]
                :as acc} row]
            {:idx (+ idx xdif)
             :hits (if (and (zero? (- idx (int idx)))
                            (= (get row (mod idx (count row))) \#))
                     (inc hits) hits)})
          {:idx 0 :hits 0}
          field))
;; (In a long-lived codebase, we'd extract lines 32-34 into let bindings.)

(def slopes [1 3 5 7 0.5])

;; Once we have that, all we gotta do is handle all the slopes,
;; extract the hits, and multiply those together.
(defn multiply-hits [field slopes]
  (->> slopes
       (map trees-hit-slope (repeat field))
       (map :hits)
       (reduce *)))

(defn -main []
  (time
    (do (println "Example (part 1):")
        (println (trees-hit-slope example 3))
        (println "Real (part 1):")
        (println (trees-hit-slope data 3))
        (println "Example (part 2):")
        (println (multiply-hits example slopes))
        (println "Real (part 2):")
        (println (multiply-hits data slopes)))))
