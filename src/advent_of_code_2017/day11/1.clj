(ns advent-of-code-2017.day11.1
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]))

(def simplifications
  {#{:ne :sw}  nil
   #{:nw :se}  nil
   #{:n  :s}   nil
   #{:ne :s}   :se
   #{:se :n}   :ne
   #{:ne :nw}  :n
   #{:n  :sw}  :nw
   #{:nw :s}   :sw
   #{:sw :se}  :s})

(defn make-simplification [curmap simp]
  (let [[mset res] simp                                   ;; e.g. [#{:ne :sw} nil]
        [d1 d2]    (seq mset)                             ;; get both directions
        d1-v       (get curmap d1)                        ;; get count for each direction
        d2-v       (get curmap d2)]
    (if (every? pos-int? [d1-v d2-v])                     ;; if we have enough of each to simplify
      (let [new-map (-> curmap
                        (assoc d1 (dec d1-v))
                        (assoc d2 (dec d2-v)))]           ;; reduce count of each direction by one
        (if res
          (assoc new-map res (inc (get new-map res 0)))   ;; if simp includes replacement, replace
          new-map))
      curmap)))

(defn run-simp
  "Run all simplifications once
   Return (possibly) simpler freq map"
  [curmap]
  (reduce make-simplification
          curmap
          simplifications))

(defn simplify
  "Run simplifications until simplest form"
  [curmap]
  (let [simpler (run-simp curmap)]
    (if (= simpler curmap)
      curmap
      (recur simpler))))

(defn count-steps [curmap]
  (reduce +
          (vals curmap)))


(defn solve-puzzle [puzz-input]
  (-> puzz-input
      frequencies
      simplify
      count-steps))

(defn get-input [inp]
  (map keyword
       (-> inp
           slurp
           str/split-lines
           first
           (str/split #","))))


(defn -main [input-filename]
  (->> input-filename
       get-input
       solve-puzzle
       println))
