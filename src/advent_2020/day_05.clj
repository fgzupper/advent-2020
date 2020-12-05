(ns advent-2020.day-05
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

(def data
  (->> (io/resource "day_05_input")
       slurp str/split-lines))

(defn seat [s]
  (let [{:keys [rub rlb r cub clb c]}
    (reduce (fn [{:keys [rub rlb r cub clb c]} ch]
              (let [rdiff (quot (- rub rlb -1) 2)
                    cdiff (quot (- cub clb -1) 2)]
                (case ch
                  \F {:rub (- rub rdiff)
                      :rlb rlb
                      :cub cub
                      :clb clb}
                  \B {:rub rub
                      :rlb (+ rlb rdiff)
                      :cub cub
                      :clb clb}
                  \L {:rub rub
                      :rlb rlb
                      :cub (- cub cdiff)
                      :clb clb}
                  \R {:rub rub
                      :rlb rlb
                      :cub cub
                      :clb (+ clb cdiff)})))
              {:rub 127 :rlb 0 :cub 7 :clb 0}
              s)]
    {:row rub :col clb}))

(defn seat-id [{:keys [row col]}]
  (+ col (* row 8)))

;; Tests
#_(seat-id (seat "FBFBBFFRLR"))
#_(seat-id (seat "BFFFBBFRRR"))
#_(seat-id (seat "FFFBBBFRRR"))
#_(seat-id (seat "BBFFBBFRLL"))

;; Solve part 1
#_(->> data
     (map seat)
     (map seat-id)
     (reduce max))


(defn next-seat [[row col]]
  (if (< col 7)
    [row (inc col)]
    [(inc row) 0]))

;; Tests
#_(next-seat [0 0])
#_(next-seat [0 7])
#_(next-seat [1 6])
#_(next-seat [1 7])

;; Solve part 2
#_(->> data
     (map seat)
     (sort-by (juxt :row :col))
     (reduce (fn [[r c] {:keys [row col] :as s}]
               (when-not (= (next-seat [r c]) [row col])
                 (println [r c] [row col]))
               [row col])
             [1 1]))
;; Kinda silly, but I looked at the println,
;; rather than returning it,
;; saw the obvious gap,
;; then called seat-id on it

(defn -main []
  (println "Evaluate in REPL for this one, sorry"))
