(ns advent-2020.day-10
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

(def data
  (->> (io/resource "day_10_input")
       slurp
       str/split-lines
       (mapv edn/read-string)))

;; Can't we just sort this?
(defn solve1 [adaptors]
  (let [sad (sort adaptors)
        m (reduce (fn [{:keys [prev gaps]} nxt]
                    {:prev nxt
                     :gaps (update gaps
                                   (- nxt prev)
                                   (fn [x] (inc (or x 0))))})
                  {:prev 0 :gaps {}}
                  sad)
        ones (get-in m [:gaps 1])
        threes (inc (get-in m [:gaps 3]))]
    (* ones threes)))

;; Part 2:
;; Every element is distinct, nice.

;; [Deleted a long section of scratchwork where I ran through an
;;  example by hand, came up with the wrong answer, but managed
;;  to figure out the basic algorithm anyway. [Hence line 53.]]

;; (There is overlap in subproblems, so we memoize with the cache.)
(defn a-help
  [cache p [f & rst]]
  (if-let [prior-answer (@cache [p f])]
    prior-answer ; If we already solved this, return cached answer.
    (let [new-answer (cond (nil? f)      0 ; Must include last one.
                           (< 3 (- f p)) 0 ; Gap between prev too big.
                           (nil? rst)    1 ; Success (all valid before, at end)
                           :else ; Not sure yet. Try with & without me:
                           (+ (a-help cache f rst)
                              (a-help cache p rst)))]
      (swap! cache assoc [p f] new-answer) ; Cache new answer.
      new-answer)))

(defn arrangements [adaptors]
  (a-help (atom {}) 0 (sort adaptors)))

#_(arrangements [2 3 5 6])
;; Wait, 5?
;; 0 [2 3 5 6] 9
;; 0 [  3 5 6] 9
;; 0 [2   5 6] 9
;; 0 [2 3   6] 9
;; 0 [  3   6] 9
;; Oh yeah, that's actually correct.
;; Cool I guess.

(defn -main []
  (time
    (let [a1 (solve1 data)
          a2 (arrangements data)]
      (println "Answer for part 1:" a1)
      (println "Answer for part 2:" a2))))
