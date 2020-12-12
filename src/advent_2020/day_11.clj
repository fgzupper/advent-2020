(ns advent-2020.day-11
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

(def data
  (->> (io/resource "day_11_input")
       slurp
       str/split-lines))

(def example
  ["L.LL.LL.LL"
   "LLLLLLL.LL"
   "L.L.L..L.."
   "LLLL.LL.LL"
   "L.LL.LL.LL"
   "L.LLLLL.LL"
   "..L.L....."
   "LLLLLLLLLL"
   "L.LLLLLL.L"
   "L.LLLLL.LL"])

;; Adjacent defined as all 8 directions.
;; Rules applied simultaneously.
;; - If empty L and NO occupied seats adjacent # --> occupied
;; - If occupied # and 4+ seats adjacent also occupied --> empty
;; This is a cellular automata, no?
;; Kinda cool iirc. Some loop forever, some are theoretically
;;  possible to use with encryption. (Press X to doubt.)

;; Let's check something real quick:
;; Columns: 0123  ; Rows:
#_(get-in ["abcd" ; 0
           "efgh" ; 1
           "ijkl"]; 2
          [1 3])
;; Good, row/column. ez.
;; [I have bad memories of screwing this up.]

(defn- adjacent [rows r-idx c-idx]
  (->> (combo/cartesian-product [-1 0 1] [-1 0 1])
       (map (fn [[rdif cdif]]
              (when-not (and (zero? rdif) (zero? cdif))
                (get-in rows [(+ r-idx rdif) (+ c-idx cdif)]))))
       (remove nil?)))
#_(adjacent example 0 0)

(defn simulate-step [rows]
  (vec
    (map-indexed
      (fn [r-idx row]
        (vec (map-indexed
               (fn [c-idx seat]
                 (or (when (= \. seat) seat)
                     (let [adjs (adjacent rows r-idx c-idx)
                           occs (->> adjs (filter (partial = \#)) count)]
                       (cond (zero? occs) \#
                             (<= 4 occs) \L
                             :else seat))))
               row)))
      rows)))

(defn simulate-all [rows]
  (loop [prev (mapv (partial mapv identity) rows)
         curr (simulate-step prev)]
 ;; I'm keeping these comments. Pretend they are log/trace statements :P
 ;;   (println "-----------")
 ;;   (run! (comp println str/join) curr)
    (if (= prev curr)
      curr
      (recur curr (simulate-step curr)))))

(defn solve [strat rows]
  (->> (strat rows)
       concat
       (map (partial filter (partial = \#)))
       (map count)
       (reduce +)))

;; This is really inefficient, FYI
#_(solve simulate-all example)
#_(solve simulate-all data)
;; (There's got to be a way to cache some things here...
;;  probably kind of like with the sums, rather than redoing
;;  the entire thing, just adjust the window.
;;  I guess a quick and dirty way would be to subvec a bit.)
;; (but doesn't that have OOB issues?)


;; Anyway, we have new rules.
;; Rather than adjacent, first in sight. (Hello Reversi!)
;; [Oh, and empty seats block sight, which I didn't realize at first.)

;; Okay, so what could we cache here?
;; I'd say for each coorinate, is it empty in a direction...
;; (but I'm not doing that yet.)
(defn sighted [rows r-idx c-idx]
  (->> (combo/cartesian-product [-1 0 1] [-1 0 1])
       (remove (partial = [0 0]))
       (filter (fn check-direction [[r-dif c-dif]]
                 (let [seat (atom \.)
                       d (atom 1)]
                   ;; This is really not idiomatic for Clojure.
                   (while (and @seat (= \. @seat))
                     (reset! seat (get-in rows [(+ r-idx (* r-dif @d))
                                                (+ c-idx (* c-dif @d))]))
                     (swap! d inc))
                   (= @seat \#))))
       count))

(def middle-empty
   [".##.##."
    "#.#.#.#"
    "##...##"
    "...L..."
    "##...##"
    "#.#.#.#"
    ".##.##."])
#_(get-in middle-empty [3 3])
#_(sighted middle-empty 3 3)
#_(sighted middle-empty 3 2)
;; Testing saves lives!
;; (Caching probably more so.)

(defn simulate-step-2 [rows]
  (vec (map-indexed
    (fn [r-idx row]
      (vec (map-indexed
        (fn [c-idx seat]
          (or (when (= \. seat) seat)
              (let [occs (sighted rows r-idx c-idx)]
                (cond (zero? occs) \#
                      (<= 5 occs) \L
                      :else seat))))
        row)))
    rows)))

(defn simulate-all-2 [rows]
  (loop [prev (mapv (partial mapv identity) rows)
         curr (simulate-step-2 prev)]
;; Same thing as before (pretend these are log/trace statements).
;;    (println "-----------")
;;    (run! (comp println str/join) curr)
    (if (= prev curr)
      curr
      (recur curr (simulate-step-2 curr)))))

#_(solve simulate-all-2 example)
#_(solve simulate-all-2 data)

(defn -main []
  (time
    (do
      (println "Part 1 example:" (solve simulate-all example))
      (println "Part 1 real:" (solve simulate-all data))
      (println "Part 2 example:" (solve simulate-all-2 example))
      (println "Part 2 real:" (solve simulate-all-2 data)))))
