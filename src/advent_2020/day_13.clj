(ns advent-2020.day-13
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

(def data
  (->> (io/resource "day_13_input")
       slurp
       str/split-lines))

;; Part 1:
(defn parse [[depart schedule]]
  {:depart (edn/read-string depart)
   :busses (->> (str/split schedule #",")
                (remove (partial = "x"))
                (map edn/read-string)
                sort)})

(defn divide? [n divisors]
  (some (fn [d] (when (zero? (mod n d)) d)) divisors))

(defn solve1 [data]
  (let [{:keys [depart busses]} (parse data)]
    (loop [t depart]
      (if-let [bus (divide? t busses)]
        (* bus (- t depart))
        (recur (inc t))))))

;; Part 2:
(defn parse2 [[_ schedule]]
  (->> (str/split schedule #",")
       (map-indexed (fn [idx s]
                      (when-not (= "x" s)
                        [idx (edn/read-string s)])))
       (remove nil?)))

(defn check [t [offset n]]
  (zero? (mod (+ t offset) n)))

;; Naive solution (don't run this at home kids)
#_(defn solve2 [data]
  (let [[[_0 start] & more] (parse2 data)]
    (some (fn [x]
            (when (every? (partial check x) more) x))
          (map (partial * start) (range)))))

;; Proper solution (hello number theory).
;; Solve for each pair to get an answer unique mod the product of the arrivals.
;; Carry that forward, and only check numbers congruent to it mod that product.
;; When you find the new answer, update the mod to multiply by the new arrival.
(defn clever-solve [data]
  (let [[[_0 start] & more] (parse2 data)]
    (loop [answer start ; First candidate
           ab-mod start ; Solution unique mod this (add it for next candidate)
           checks [[0 start]] ; First check (add these gradually)
           busses more]
      (if-let [next-bus (first busses)]
        (let [new-checks (cons next-bus checks) ; include next check in list
              new-answer (some #(when (every? (partial check %) new-checks) %)
                               (->> (range) ; infinite lazy sequence
                                    (map (partial * ab-mod)) ; 0*ab, 1*ab, ...
                                    (map (partial + answer))))] ; + last answer
          (recur new-answer
                 (* ab-mod (second next-bus))
                 new-checks
                 (rest busses)))
        ;; Base case (no more checks):
        answer))))

(defn -main []
  (time
    (let [a1 (solve1 data)
          a2 (clever-solve data)]
      (println "Answer to part 1:" a1)
      (println "Answer to part 2:" a2))))
