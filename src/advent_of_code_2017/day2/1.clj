(ns advent-of-code-2017.day2.1
  (:require [clojure.string :as str]))

(defn minmax
  ([xs] (minmax (first xs) (first xs) xs))
  ([mi ma xs]
   (let [x   (first xs)
         nmi (min mi x)
         nma (max ma x)
         nxt (next xs)]
     (if nxt
       (recur nmi nma nxt)
       [nmi nma]))))

(defn max-min-diff [row]
  (let [[mi mx] (minmax row)]
    (- mx mi)))

(defn solve-puzzle [input]
  (reduce +
          (map max-min-diff
               input)))

(defn process-input [input-filename]
  (->> input-filename
       slurp
       str/split-lines
       (map #(str/split % #"\t")) ;; come on, tabs!?
       (map (fn [line]
              (map #(Integer. %) line))))) ;; convert strings to Integers

(defn -main [input-filename]
  (->> input-filename
       process-input
       solve-puzzle
       println))
