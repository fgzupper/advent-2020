(ns advent-2020.day-15
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [clojure.pprint :as pp]
            [clojure.string :as str]))

;; Small enough to inline:
(def data [9,12,1,4,17,0,18])

(defn- two-queue [[prev _] now]
  (if prev [now prev] [now]))

(defn play-with-elves [starting-numbers max-turns]
  (loop [start starting-numbers
         cache {} ; we could count flattened vals here
         turn 1 ; instead of turns here, but that cray
         prev nil]
    (if-let [f (first start)]
      (recur (rest start)
             (update cache f two-queue turn)
             (inc turn)
             f)
      (if (> turn max-turns)
        prev
        (let [[n n-1] (cache prev)
              speak-now (if n-1 (- n n-1) 0)]
          (when (zero? (mod turn 1000000))
            ;; (When here for a while, let user know progress.)
            (println "[turn:" turn "]"))
          (recur nil
                 (update cache speak-now two-queue turn)
                 (inc turn)
                 speak-now))))))

(defn -main []
  (time
    (do
      (println "Part 1:" (play-with-elves data 2020))
      (println "[one minute please]")
      (println "Part 2:" (play-with-elves data 30000000)))))
