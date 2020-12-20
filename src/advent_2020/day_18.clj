(ns advent-2020.day-18
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [clojure.pprint :as pp]
            [clojure.string :as str]))

(def data
  (->> "day_18_input" io/resource slurp
       str/split-lines))

(def numeric-chars #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})

;; Want a stream of [0 12] [0 +] [1 3] [1 *] [1 2]
;; (First number is depth; bigger depths get processed first.)
(defn ->tokens [line]
  (->> (str line " ")
       (reduce (fn [{:keys [depth num-str coll] :as acc} next-ch]
                 (if (numeric-chars next-ch)
                   (update acc :num-str str next-ch)
                   (let [acc2
                         (if num-str
                           (-> acc
                               (update :coll conj [depth (edn/read-string num-str)])
                               (dissoc :num-str))
                           acc)]
                     (case next-ch
                       \( (update acc2 :depth inc)
                       \) (update acc2 :depth dec)
                       \* (update acc2 :coll conj [depth *])
                       \+ (update acc2 :coll conj [depth +])
                       acc2))))
               {:depth 0 :coll []})
       :coll))

;; Process tokens furthest in parens first (max depth)
(defn math-tokens [tokens]
  (let [max-depth (->> tokens (map first) (reduce max))
        result
        (reduce (fn [{:keys [prev op n-tokens] :as acc}
                     [depth num-or-op :as now]]
                  (if (< depth max-depth)
                    (if prev
                      (-> acc
                          (dissoc :prev)
                          (dissoc :op)
                          (update :n-tokens conj [(dec max-depth) prev])
                          (update :n-tokens conj now))
                      (update acc :n-tokens conj now))
                    (if (fn? num-or-op)
                      (assoc acc :op num-or-op)
                      (if op
                        (-> acc
                            (update :prev op num-or-op)
                            (dissoc op))
                        (assoc acc :prev num-or-op)))))
                {:n-tokens []}
                tokens)]
    (if (zero? max-depth)
      (:prev result)
      (if-let [p (:prev result)]
        (conj (:n-tokens result) [(dec max-depth) p])
        (:n-tokens result)))))
;; I could have str'd with "+ 0 " to get past this, but that's silly.

(defn math-all-tokens [tokens]
  (loop [tkns tokens]
    (if (number? tkns)
      tkns
      (recur (math-tokens tkns)))))

;; Part 2: give addition greater precedence.

;; If we're out of parens, we put back a value.
;; If we're at the end of a series of ops, the depth is reduced.
(defn handle-outside-parens
  [{:keys [prev n-tokens] :as acc} at-end? max-depth]
  (if prev
    (-> acc
        (dissoc :prev)
        (dissoc :op)
        (update :n-tokens
                conj
                [(if at-end? (dec max-depth) max-depth) prev]))
    acc))

(defn math-tokens-3 [ops tokens]
  (loop [n-tokens tokens
         rmng-ops ops]
    (let [op-todo (first rmng-ops)
          max-depth (->> n-tokens (map first) (reduce max))
          rmng-ops  (rest rmng-ops)
          at-end?   (empty? rmng-ops)
          result
          (reduce (fn [{:keys [prev op n-tokens not-done] :as acc}
                       [depth num-or-op :as now]]
                    (if (< depth max-depth)
                      (-> (handle-outside-parens acc at-end? max-depth)
                          (update :n-tokens conj now))
                      (if (fn? num-or-op)
                        (assoc acc :op num-or-op)
                        (if op
                          (if (= op op-todo)
                            (-> acc
                                (update :prev op num-or-op)
                                (dissoc op))
                            (-> acc ; not ours, put it back!
                                (dissoc :op)
                                (update :n-tokens conj [max-depth prev])
                                (update :n-tokens conj [max-depth op])
                                (assoc :prev num-or-op)))
                          (assoc acc :prev num-or-op)))))
                  {:n-tokens []}
                  n-tokens)
          result-v2
          (if (empty? (:n-tokens result))
            (:prev result)
            (:n-tokens
              (if-let [p (:prev result)]
                (handle-outside-parens result at-end? max-depth)
                result)))]
      (if (or (number? result-v2) at-end?)
        result-v2
        (recur result-v2 rmng-ops)))))

(def op-precedence [+ *])
(defn math-all-tokens-3 [tokens]
  (loop [tkns tokens]
    (if (number? tkns)
      tkns
      (recur (math-tokens-3 op-precedence tkns)))))

(defn -main []
  (time
    (let [tokens (map ->tokens data)]
      (println "Answer to part 1:")
      (println (->> tokens (map math-all-tokens) (reduce +)))
      (println "Answer to part 2:")
      (println (->> tokens (map math-all-tokens-3) (reduce +))))))
