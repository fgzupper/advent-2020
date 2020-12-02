(ns advent-2020.day-02
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

;; could io/resource, but that'd require a repl restart
(def data
  (->> "resources/day_02_input"
       slurp str/split-lines))

(def example-data
  ["1-3 a: abcde"
   "1-3 b: cdefg"
   "2-9 c: ccccccccc"
   ])

#_(str/split "1-3 a: abcde" #" ")

(defn valid? [pw]
  (let [[rng ltr wrd] (str/split pw #" ")
        [lb ub] (->> (str/split rng #"-") (map edn/read-string))
        let (first ltr)
        chars (->> wrd (filter (partial = let)) count)]
    (<= lb chars ub)))

#_(map valid? example-data)
#_(->> data (filter valid?) count)

(defn valid-2? [pw]
  (let [[rng ltr wrd] (str/split pw #" ")
        [lb ub] (->> (str/split rng #"-") (map edn/read-string))
        let (first ltr)
        t (= (get wrd (dec lb)) let)
        r (= (get wrd (dec ub)) let)
        ]
    (or (and t (not r))
        (and (not t) r))))
#_(valid-2? "1-3 a: abcde")
#_(->> data (filter valid-2?) count)

;; Eh, not cleaning this one up much. It's just string logic.
;; I solved it by evaluating the forms above.
;; But here's a main method:
(defn -main []
  (time
    (do
      (println "Answer one:" (->> data (filter valid?) count))
      (println "Answer two:" (->> data (filter valid-2?) count)))))
