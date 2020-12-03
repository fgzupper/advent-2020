(ns advent-2020.day-03
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

;; could io/resource, but that'd require a repl restart
(def data
  (->> "resources/day_03_input"
       slurp str/split-lines
       ))

(def example-data
  [
   "..##......."
"#...#...#.."
".#....#..#."
"..#.#...#.#"
".#...##..#."
"..#.##....."
".#.#.#....#"
".#........#"
"#.##...#..."
"#...##....#"
".#..#...#.#"
   ])

;; y increase 1, x increase 3
;; (or just x increase 3)
(defn trees-hit [field]
  (reduce (fn [{:keys [idx hits]
                :as acc} row]
            {:idx (+ idx 3)
             :hits (if (= (get row (mod idx (count row))) \#) (inc hits) hits)})
          {:idx 0 :hits 0}
          field))

(trees-hit example-data)
(trees-hit data)
(trees-hit ["#..."])
(trees-hit ["#..."
            "...#"])

(- 0.5 (int 0.5))
(- 1 (int 1))
(defn trees-hit-slope [field xdif]
  (reduce (fn [{:keys [idx hits]
                :as acc} row]
            {:idx (+ idx xdif)
             :hits (if (and (zero? (- idx (int idx)))
                            (= (get row (mod idx (count row))) \#))
                     (inc hits) hits)})
          {:idx 0 :hits 0}
          field))

(trees-hit-slope data 3)
(trees-hit-slope data 3)

(->> [1 3 5 7 0.5]
     (map trees-hit-slope (repeat data))
;     (map trees-hit-slope (repeat example-data))
     (map :hits)
     (reduce *))

(defn -main []
  (time
    (do
        )))
