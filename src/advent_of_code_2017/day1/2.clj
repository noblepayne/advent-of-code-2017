(ns advent-of-code-2017.day1.2
  (:require [advent-of-code-2017.day1.1 :as day1-1]
            [clojure.string :as str]))

(defn solve-captcha [captcha]
  (let [len (count captcha)
        half (/ len 2)
        len-plus-half (+ len half)]
    (->> captcha
         vec
         cycle
         (take len-plus-half)
         (partition (inc half) 1)
         (filter
          #(= (first %) (last %)))
         (map day1-1/digit->int)
         (reduce +))))

(defn -main [input-filename]
  (-> input-filename
      slurp
      str/split-lines
      first
      solve-captcha
      println))
