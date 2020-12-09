(ns advent-2020.day-08
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

(def data
  (-> (io/resource "day_08_input") slurp
      (str/split-lines)
      vec))

(defn parse-instruction [s]
  (let [[cmd n] (str/split s #"\s")]
    [cmd (edn/read-string n)]))

;; Run until we hit the same node twice:
(defn emulate [instructions]
  (loop [idx 0
         visited #{}
         acc 0]
    (if (visited idx)
      acc
      (let [[cmd n] (parse-instruction (get instructions idx))]
        (case cmd
          "nop" (recur (inc idx) (conj visited idx) acc)
          "acc" (recur (inc idx) (conj visited idx) (+ acc n))
          "jmp" (recur (+ idx n) (conj visited idx) acc))))))

;; Part 2... switching a NOP with JMP
;; (or the opposite)
;; [yeah, just jump to end? there's got to be more to this.]
;; Oh right, you don't get to change the number.
;; Alright, we've got a real problem on our hands then.
(defn- new-args [idx visited acc cmd n]
  (case cmd
    "nop" [(inc idx) (conj visited idx) acc]
    "acc" [(inc idx) (conj visited idx) (+ acc n)]
    "jmp" [(+ idx n) (conj visited idx) acc]))

(def legal-swaps
  {"nop" "jmp"
   "jmp" "nop"})

;; Added change?, which tracks if we made our one legal cmd change yet.
(defn- helper [idx visited acc change?]
  (cond
    (nil? (get data idx)) acc ; success
    (visited idx) false       ; fail, undo a choice
    :else                     ; normal stuff (recursion)
    (let [[cmd n] (parse-instruction (get data idx))
          [i v a] (new-args idx visited acc cmd n)]
          (or (helper i v a change?)
              (when (and (not change?) (legal-swaps cmd))
                (let [[i2 v2 a2 :as args]
                      (new-args idx visited acc (legal-swaps cmd) n)]
                  (helper i2 v2 a2 true)))))))

(defn emulate2 [instructions]
  (helper 0 #{} 0 false))

(defn -main []
  (time
    (do
      (println "Answer for part 1:")
      (println (emulate data))
      (println "Answer for part 2:")
      (println (emulate2 data)))))
;; nice!
