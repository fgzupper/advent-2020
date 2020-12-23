(ns advent-2020.day-19
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [clojure.pprint :as pp]
            [clojure.string :as str]))

(def data
  (-> "day_19_input"
      io/resource slurp
      (str/split #"\n\n")))

(defn load-rules [[rules _messages]]
  (->> (str/split-lines rules)
       (map #(str/split % #": "))
       (map (fn [[i s]] [(edn/read-string i) s]))
       (sort-by first)
       (map second)
       (map #(str/split % #" \| "))
       (map (partial map #(str/split % #" ")))
       (mapv (partial mapv (partial mapv edn/read-string)))))
;; Sort the lines by index, then march through some transformations:
;; '(  "8 11"   "39 64 | 110 44"   "124 110 | 88 39"  ...)
;; '( ["8 11"] ["39 64" "110 44"] ["124 110" "88 39"] ...)
;; '( [[8 11]] [[39 64] [110 44]] [[124 110] [88 39]] ...)

;; Part 1 literally describes regular expressions,
;;  so let's turn it into a literal regular expression :P
(defn rule-to-regex-str [cache rules idx]
  (if-let [re (@cache idx)]
    re
    (let [[[ffirst-v] :as all] (get rules idx)]
      (if (string? ffirst-v)
        ((swap! cache assoc idx ffirst-v) idx)
        (->> all ; vec of vec of indices --> vec of vec of regex
             (map (partial map (partial rule-to-regex-str cache rules)))
             (map (partial map (fn [re-str] (str "(?:" re-str ")"))))
             (map str/join) ; concatenate inner vector elements
             (map (fn [re-str] (str "(?:" re-str ")")))
             (str/join "|")
             (swap! cache assoc idx)
             (#(get % idx)))))))

(defn solve-1 [data]
  (let [rules (load-rules data)
        cache (atom {})
        regex-str (rule-to-regex-str cache rules 0)
        regex (re-pattern (str "^" regex-str "$"))]
    (->> (second data)
         str/split-lines
         (filter (partial re-find regex))
         count)))

;; Part 2
(defn stupid [re-str-42 re-str-31 lb ub]
  (->> (range lb ub)
       (map (fn [i] (str "(?:(?:" re-str-42 "){" i "}"
                            "(?:" re-str-31 "){" i "})")))
       (str/join "|")))

(defn solve-2 [data]
  (let [rules (load-rules data)
        cache-1 (atom {})
        _ (rule-to-regex-str cache-1 rules 0)
        cache-1-map @cache-1
        re-str-8 (cache-1-map 8)
        re-str-42 (cache-1-map 42)
        re-str-31 (cache-1-map 31)
        cache-2-map {8 (str "(?:" re-str-8 ")+")
                     11 (stupid re-str-42 re-str-31 1 10)}
        cache-2 (atom cache-2-map)
        regex-str (rule-to-regex-str cache-2 rules 0)
        regex (re-pattern (str "^" regex-str "$"))]
    (->> (second data)
         str/split-lines
         (filter (partial re-matches regex))
         count)))

(defn -main []
  (time
    (let [a1 (solve-1 data)
          a2 (solve-2 data)]
      (println "Answer to part 1:" a1)
      (println "Answer to part 2:" a2))))
