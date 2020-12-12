(ns advent-2020.day-12
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

(def data
  (->> (io/resource "day_12_input")
       slurp
       str/split-lines))

;; part 1
(defn parse-i [s]
  [(re-find #"[A-Z]" s) 
   (edn/read-string (re-find #"\d+" s))])

(defn handle-f [x y deg units]
  [(+ x (* (Math/cos (Math/toRadians deg)) units))
   (+ y (* (Math/sin (Math/toRadians deg)) units))])

(defn update-position [ins]
  (->> ins
       (map parse-i)
       (reduce (fn [[x y deg] [cmd units]]
                 (case cmd
                   "N" [x (+ y units) deg]
                   "S" [x (- y units) deg]
                   "E" [(+ x units) y deg]
                   "W" [(- x units) y deg]
                   "F" (conj (handle-f x y deg units) deg)
                   "R" [x y (- deg units)]
                   "L" [x y (+ deg units)]))
               [0 0 0])))
 
;; part 2
(defn handle-f-2 [x y sx sy units]
  (let [xd (- x sx)
        yd (- y sy)
        xc (* xd units)
        yc (* yd units)]
   [(+ x xc) (+ y yc) (+ sx xc) (+ sy yc)]))

(defn rotate [x y sx sy drad]
  (let [xd (- x sx)
        yd (- y sy)
        rad1 (if (zero? xd)
               (if (pos? yd)
                 (/ Math/PI 2)
                 (/ (* 3 Math/PI) 2))
               (if (neg? xd)
                 (+ Math/PI (Math/atan (/ yd xd)))
                 (Math/atan (/ yd xd))))
        rad2 (mod (+ rad1 (Math/toRadians drad)) (* 2 Math/PI))
        dist (Math/sqrt (+ (* xd xd) (* yd yd)))]
  [(+ sx (* (Math/cos rad2) dist))
   (+ sy (* (Math/sin rad2) dist))
   sx sy]))

(defn update-position-2 [ins]
  (->> ins
       (map parse-i)
       (reduce (fn [[x y sx sy] [cmd units]]
                 (case cmd
                   "N" [x (+ y units) sx sy]
                   "S" [x (- y units) sx sy]
                   "E" [(+ x units) y sx sy]
                   "W" [(- x units) y sx sy]
                   "F" (handle-f-2 x y sx sy units)
                   "R" (rotate x y sx sy (- units))
                   "L" (rotate x y sx sy units)))
               [10 1 0 0])))

(defn -main []
  (time
    (let [a1 (let [[x y] (update-position data)]
               (+ (Math/abs x) (Math/abs y)))
          a2 (let [[_ _ x y] (update-position-2 data)]
               (+ (Math/abs x) (Math/abs y)))]
      (println "Answer to 1:" a1)
      (println "Answer to 2:" a2))))
