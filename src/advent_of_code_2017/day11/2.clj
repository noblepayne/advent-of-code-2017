(ns advent-of-code-2017.day11.2
  (:require [advent-of-code-2017.day11.1 :as day11-1]))

(defn make-input-list [input]
  "List of all subsets of input (including itself)"
  (map #(take (inc %)
              input)
       (range (count input))))

(defn solve-puzzle [input]
  (->> input
       make-input-list
       (map day11-1/solve-puzzle)
       (reduce max)))

(defn -main [input-filename]
  (->> input-filename
       day11-1/get-input
       solve-puzzle
       println))
