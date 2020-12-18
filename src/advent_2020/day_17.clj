(ns advent-2020.day-17
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [clojure.pprint :as pp]
            [clojure.string :as str]))

;; Small enough to inline
(def data
  ["####...#"
   "......#."
   "#..#.##."
   ".#...#.#"
   "..###.#."
   "##.###.."
   ".#...###"
   ".##....#"])

(defn data->starting-state
  [data]
  (->> data
       (map-indexed
         (fn [y xs]
           (map-indexed
             (fn [x itm]
               (when (= \# itm)
                 [[x y 0] true]))
             xs)))
       (apply concat)
       (remove nil?)
       (into {})))

(let [diffs (->> (repeat 3 [-1 0 1])
                 (apply  combo/cartesian-product)
                 (remove (partial = [0 0 0])))]
  (defn neighbors [state coordinate]
    (->> (map (partial map + coordinate) diffs)
         (reduce (fn [acc coord]
                   (if (state coord)
                     (update acc :on  conj coord)
                     (update acc :off conj coord)))
                 {:on #{} :off #{}}))))

(defn sim-cycle [state]
  (let []
    (loop [on-todo (-> state keys set)
           off-todo #{}
           done #{}
           next-state {}]
      (cond
        (seq on-todo) ; process all active
        (let [coord (first on-todo)
              {:keys [on off]} (neighbors state coord)
              new-off-todo (set/difference off done)
              on-now? (<= 2 (count on) 3)]
          (recur (rest on-todo) ; okay for this to be list
                 (set/union off-todo new-off-todo)
                 (conj done coord)
                 (if on-now?
                   (assoc next-state coord true)
                   next-state)))
        (seq off-todo) ; process all adjacent inactive
        (let [coord (first off-todo)
              {:keys [on off]} (neighbors state coord)
              on-now? (= 3 (count on))]
          (recur on-todo ; will be empty
                 (rest off-todo)
                 (conj done coord) ; irrelevant, but we do anyway
                 (if on-now?
                   (assoc next-state coord true)
                   next-state)))
        :else ; at the end, yay.
        next-state))))

(defn sim-n-cycles [n state]
  (reduce (fn [acc _x] (sim-cycle acc))
         state
         (range n)))

;; Part 2
;; (Yes I could parameterize part 1 and reuse.)
;; (No I'm not doing that... at least not yet.)
(defn data->starting-state-4d
  [data]
  (->> data
       (map-indexed
         (fn [y xs]
           (map-indexed
             (fn [x itm]
               (when (= \# itm)
                 [[x y 0 0] true]))
             xs)))
       (apply concat)
       (remove nil?)
       (into {})))

(let [diffs (->> (repeat 4 [-1 0 1])
                 (apply  combo/cartesian-product)
                 (remove (partial = [0 0 0 0])))]
  (defn neighbors-4d [state coordinate]
    (->> (map (partial map + coordinate) diffs)
         (reduce (fn [acc coord]
                   (if (state coord)
                     (update acc :on  conj coord)
                     (update acc :off conj coord)))
                 {:on #{} :off #{}}))))

(defn sim-cycle-4d [state]
  (let []
    (loop [on-todo (-> state keys set)
           off-todo #{}
           done #{}
           next-state {}]
      (cond
        (seq on-todo) ; process all active
        (let [coord (first on-todo)
              {:keys [on off]} (neighbors-4d state coord)
              new-off-todo (set/difference off done)
              on-now? (<= 2 (count on) 3)]
          (recur (rest on-todo) ; okay for this to be list
                 (set/union off-todo new-off-todo)
                 (conj done coord)
                 (if on-now?
                   (assoc next-state coord true)
                   next-state)))
        (seq off-todo) ; process all adjacent inactive
        (let [coord (first off-todo)
              {:keys [on off]} (neighbors-4d state coord)
              on-now? (= 3 (count on))]
          (recur on-todo ; will be empty
                 (rest off-todo)
                 (conj done coord) ; irrelevant, but we do anyway
                 (if on-now?
                   (assoc next-state coord true)
                   next-state)))
        :else ; at the end, yay.
        next-state))))

(defn sim-n-cycles-4d [n state]
  (reduce (fn [acc _x] (sim-cycle-4d acc))
         state
         (range n)))

(defn -main []
  (time
    (let [init (data->starting-state data)
          a1 (count (sim-n-cycles 6 init))
          _ (println "Part 1:" a1)
          init-4d (data->starting-state-4d data)
          a2 (count (sim-n-cycles-4d 6 init-4d))
          _ (println "Part 2:" a2)])))
