(ns advent-2020.day-14
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [clojure.pprint :as pp]
            [clojure.string :as str]))

(def data
  (->> (io/resource "day_14_input")
       slurp
       str/split-lines))

(defn parse-mask [s]
  (when (str/starts-with? s "mask")
    (-> s (str/split #" = ") last)))

(defn parse-mem [s]
  (let [[mem n] (str/split s #" = ")
        address (re-find #"\d+" mem)]
    (mapv edn/read-string [address n])))

(defn mask-lines [lines]
  (reduce
    (fn [[acc mask] s]
      (if-let [m (parse-mask s)]
        [acc m]
        (let [[address n] (parse-mem s)
              and-mask (->> (str/replace mask "X" "1") 
                            (str "2r")
                            edn/read-string)
              or-mask (->> (str/replace mask "X" "0") 
                           (str "2r")
                           edn/read-string)
              masked (-> n (bit-and and-mask) (bit-or or-mask))]
          [(assoc acc address masked) mask])))
    [{} nil]
    lines))

(defn sum-memory [[memory _last-mask]]
  (->> memory vals (reduce +)))

;; Part 2 writes the same value to multiple places.
;; That's kinda weird, but whatever.
(defn weird-mask [mask s]
  (->> (map vector mask s)
       (reduce
         (fn [acc [m c]]
           (conj acc
                 (case m
                   \X [\0 \1]
                   \0 [c]
                   \1 [\1])))
         [])
       (apply combo/cartesian-product)
       ;; Technically you don't need to convert back to numbers, but we will:
       (map str/join)
       (map (partial str "2r"))
       (map edn/read-string)))

(defn binary-str [n] ; number -> 36-length binary string.
  (pp/cl-format nil "~36,'0',B" n))

(defn solve-2 [lines]
  (reduce
    (fn [[acc mask] s]
      (if-let [m (parse-mask s)]
        [acc m]
        (let [[address n] (parse-mem s)
              addresses (weird-mask mask (binary-str address))]
          [(reduce #(assoc %1 %2 n) acc addresses) mask]
          )))
    [{} nil]
    lines))

(defn -main []
  (time
    (let [a1 (-> data mask-lines sum-memory)
          a2 (-> data solve-2 sum-memory)]
      (println "Answer to part 1:" a1)
      (println "Answer to part 2:" a2))))
