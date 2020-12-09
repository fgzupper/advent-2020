(ns advent-2020.day-09
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

(def data
  (->> (io/resource "day_09_input")
       slurp
       str/split-lines
       (mapv edn/read-string)))

;; Modified linear recurrence relation.
;; But of 25 variables, probably to discourage solutions that involve
;; differential equations. (I'm not about to mess with 25 variables.)
;; Guess for modification -- different length of preamble.
(defn sum-to-target? [target numbers] ; modified d01 fn
  (let [s (set numbers)]
    (some (fn [x] (s (- target x))) numbers)))

(defn find-invalid [numbers]
  (loop [idx 25]
    (let [n (get numbers idx)
          v (subvec numbers (- idx 25) idx)]
      (if (sum-to-target? n v)
        (recur (inc idx))
        n))))

;; Part 2: subseq that sums to invalid.
;; (Rats, wasn't the modification that I expected.)
(defn find-weakness [numbers secret]
  (let [cnt (count numbers)]
    (loop [s-idx 0 ; inclusive
           e-idx 2 ; exclusive
           grow? true
           prevs (get numbers s-idx)]
      (when (<= e-idx cnt) ; OOB -> stop
        (let [v (subvec numbers s-idx e-idx)
              sum (if grow?
                    (+ prevs (get numbers (dec e-idx)))
                    (- prevs (get numbers (dec s-idx))))]
          (cond ; check for just right, too big, or too small.
            (= secret sum) (let [mx (reduce max v)
                                 mn (reduce min v)]
                             (+ mx mn)) ; weakness found
            (< secret sum) (recur (inc s-idx) e-idx false sum)
            :else (recur s-idx (inc e-idx) true sum)))))))

(defn -main []
  (time
    (let [a1 (find-invalid data)
          a2 (find-weakness data a1)]
      (println "Answer to part 1:" a1)
      (println "Answer to part 2:" a2))))
