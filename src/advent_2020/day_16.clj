(ns advent-2020.day-16
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [clojure.pprint :as pp]
            [clojure.string :as str]))

(def data
  (-> (io/resource "day_16_input")
      slurp
      (str/split #"\n\n")))

(defn range-to-set [s]
  (let [[lb ub] (->> (str/split s #"-") (map edn/read-string))]
    (set (range lb (inc ub)))))

;; Not maintaining all information yet,
;; just that which is relevant for part 1.
(defn parse-header [header]
  (->> (str/split-lines header)
       (map (fn [line]
              (let [[tag ranges] (str/split line #": ")
                    [r1 r2] (str/split ranges #" or ")]
                (set/union (range-to-set r1)
                           (range-to-set r2)))))
       (reduce set/union)))

(defn parse-nearby [nearby]
  (mapv edn/read-string
        (-> nearby
            (str/replace-first "nearby tickets:\n" "")
            (str/split #",|\n"))))

(defn invalid-nearby [[header _ticket nearby]]
  (let [range-unions (parse-header header)
        nearby (parse-nearby nearby)]
    (->> nearby
         (remove range-unions)
         (reduce +))))

;; Part 2:
(defn parse-header-2 [header]
  (->> (str/split-lines header)
       (map (fn [line]
              (let [[tag ranges] (str/split line #": ")
                    [r1 r2] (str/split ranges #" or ")]
                {tag (set/union (range-to-set r1)
                                (range-to-set r2))})))
       (reduce merge)))

(defn parse-ticket [ticket]
  (-> ticket
      (str/replace-first "your ticket:\n" "")
      (str/split #",")
      (->> (mapv edn/read-string))))

(defn parse-nearby-2 [nearby]
  (->> (-> nearby
           (str/replace-first "nearby tickets:\n" "")
           str/split-lines)
       (map #(str/split % #","))
       (map (partial mapv edn/read-string))))

(defn trim-sets-per-ticket
  [field->allowed-values field-sets ticket]
  (map (fn [field-set ticket-num]
         (->> field-set
              (filter (fn [f] ((field->allowed-values f) ticket-num)))
              set))
       field-sets
       ticket))

;; Now that we have trimmed down the sets of allowed fields per position,
;; (as one does with a sudoku puzzle), we can solve it with backtracking.
(defn one-per-set [[next-set & after-sets :as all-sets]]
  (cond (nil? next-set)  '()   ; successfully reached end!
        (= #{} next-set) false ; no valid options, undo a choice
        :else
        (let [choice (first next-set)
              others (rest next-set)
              result (one-per-set (map disj after-sets (repeat choice)))]
          (if result
            (cons choice result)
            (when (seq others)
              (one-per-set (cons (disj next-set choice) after-sets)))))))

;; This glues the two together to figure out the order of fields,
;;  then parse your ticket into a matching map.
(defn get-ticket-map [[header ticket nearby]]
  (let [field->allowed-values (parse-header-2 header)
        allowed-union (->> field->allowed-values vals (reduce set/union))
        ticket (parse-ticket ticket)
        valid-nearby (->> nearby
                          parse-nearby-2
                          (filter (partial every? allowed-union)))
        initial-sets (->> field->allowed-values keys set
                          (repeat (count ticket)))
        refined-sets (reduce (partial trim-sets-per-ticket
                                        field->allowed-values)
                             initial-sets
                             valid-nearby)
        single-ordering (one-per-set refined-sets)
        ticket-map (zipmap single-ordering ticket)]
    ticket-map))

(defn -main []
  (time
    (let [a1 (invalid-nearby data)
          a2 (->> (get-ticket-map data)
                  (keep (fn [[k v]] (when (str/starts-with? k "departure") v)))
                  (reduce *))]
      (println "Answer to part 1:" a1)
      (println "Answer to part 2:" a2))))
