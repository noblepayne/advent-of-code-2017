(ns advent-of-code-2017.day21.1
  (:require [clojure.repl :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.java.io :as io]
            [clojure.core.matrix :as m]))


(def starting-grid
  [[\. \# \.]
   [\. \. \#]
   [\# \# \#]])

(def test-grid-1
  [[\. \# \. \#]
   [\. \. \# \.]
   [\# \# \# \.]
   [\. \. \# \.]])

(def test-grid-2
  [[\. \# \. \# \. \#]
   [\. \. \# \. \. \#]
   [\# \# \# \. \. \#]
   [\. \. \# \. \. \#]
   [\# \. \. \. \# \#]
   [\. \# \. \. \# \.]])

(defn process-segment [segment]
  (mapv vec
        (str/split segment
                   #"/")))

(defn process-line [line]
  (mapv process-segment line))

(defn get-input [filename]
  (->> filename
       slurp
       str/split-lines
       (map #(str/split % #" => "))
       (map process-line)
       (into {})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn size [matrix]
  (count (first matrix)))

(defn two-or-three [matrix]
  (let [s (size matrix)]
    (if (zero? (mod s 2))
      2
      3)))

(defn partitionv [n coll]
  (->> coll
       (partition n)
       (mapv vec)))

(defn reverse-matrix [m]
  (mapv (comp vec reverse) m))

(defn rotate-right [m]
  (-> m m/transpose reverse-matrix))

(defn rotations [m]
  (take 4 (iterate rotate-right m)))

(defn match-options [m]
  (concat (rotations m) (rotations (reverse-matrix m))))

(defn find-match [match-map m]
  (let [opts (match-options m)]
    (some #(get match-map %) opts)))

(defn print-matrix [m]
  (run! println m))

(defn print-matricies [ml]
  (run! #(do (print-matrix %) (println)) ml))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-sub-partitions [matrix part-by]
  (->> matrix
       (mapv #(partitionv part-by %))
       (partitionv part-by)))

(defn make-sub-matricies [subsize numsub matrix]
  (let [partitions (make-sub-partitions matrix subsize)]
    (for [x (range numsub)
          y (range numsub)]
      (mapv #(nth % y)
            (nth partitions
                 x)))))

(defn run-enhancements [match-map subsize numsub matrix]
  (->> matrix
       (make-sub-matricies subsize numsub)
       (map #(find-match match-map %))))

(defn combine-subgroup [subgroup]
  (->> subgroup
       (apply (partial map concat))
       (mapv vec)))

(defn combine-submatricies [n submatricies]
  (->> submatricies
       (partition n)
       (map combine-subgroup)
       (apply concat)
       vec))


(defn run-iteration [match-map matrix]
  (let [size       (size matrix)
        subsize    (two-or-three matrix)
        numsub     (/ size subsize)]
    (->> matrix
         (run-enhancements match-map subsize numsub)
         (combine-submatricies numsub))))

(defn nth-iteration [match-map start n]
  (nth (iterate (partial run-iteration match-map) start) n))

(defn count-on [matrix]
  (->> matrix
       flatten
       (filter #{\#})
       count))
