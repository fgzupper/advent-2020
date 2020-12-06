(ns advent-2020.day-06
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

;; Alright, today we're going to go for more up-front thinking,
;;  and less typing like a madman.
;; The last one definitely had a trick that I missed,
;;  and might have caught with 5 more minutes of thinking.
;; [oh, this is like binary --> wait, this is literally binary]

(def example
  [
   ])

(def data
  (-> (io/resource "day_06_input")
      slurp (str/split #"\n\n")))

  ;; Count the number of questions to which anyone answered yes
  ;; Sum them
(defn yescount [group]
  (->> (str/replace group #"\s*" "")
       (map identity)
       (into #{})
       (count)))


(defn yescount2 [group]
  (->> (str/split-lines group)
       (map #(str/replace % #"\s*" ""))
       (map (partial map identity))
       (map (partial into #{}))
       (reduce clojure.set/intersection)
       (count)))

(defn -main []
  (time
    (do
      (->> data
           (map yescount)
           (reduce +)
           println)
      (->> data
           (map yescount2)
           (reduce +)
           println))))
;; Haha, said I'd think, and this one was trivial. Figures.
