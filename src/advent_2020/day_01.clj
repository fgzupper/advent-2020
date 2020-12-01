(ns advent-2020.day-01
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

(def data
  (->> "resources/day_01_input"
       slurp str/split-lines
       (map edn/read-string)))

(defn solve2 [target numbers]
  (let [s (set numbers)]
    (->> numbers
         (some (fn [x]
                 (let [c (- target x)]
                   (when (s c)
                     (* x c))))))
  ))

(def example
  [1721
   979
   366
   299
   675
   1456])

(defn solve3 [target numbers]
  (let [s (set numbers)]
    (->> numbers
         (some (fn [x]
                 (let [c (- target x)
                       prod2 (solve2 c s)]
                   (when prod2
                     (* x prod2))))))))

;; So, we need to find two numbers that sum to a number

(defn -main []
  (time
    (do
      (println "Check example for part 1:")
      (println (solve2 2020 example))
      (println "Real thing for part 1:")
      (println (solve2 2020 data))
      (println "Check example for part 2:")
      (println (solve3 2020 example))
      (println "Real thing for part 2:")
      (println (solve3 2020 data)))))
