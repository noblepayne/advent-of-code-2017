(ns advent-of-code-2017.day2.2
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [advent-of-code-2017.day2.1 :as day2-1]))

(defn special-divide [row]
  (let [[mi ma] (day2-1/minmax row)]
    (/ ma mi)))

(defn sum-row [row]
  (reduce +
          (filter int?
                  (map special-divide (combo/combinations row 2)))))

(defn solve-puzzle [input]
  (reduce +
          (map sum-row input)))

(defn -main [input-filename]
  (->> input-filename
       day2-1/process-input
       solve-puzzle
       println))
