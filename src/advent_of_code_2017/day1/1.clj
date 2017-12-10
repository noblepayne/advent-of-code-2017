(ns advent-of-code-2017.day1.1
  (:require [clojure.string :as str]))

(defn digits-match? [[d1 d2]]
  (= d1 d2))

(def digit->int (comp #(- % 48)
                      int
                      first))

(defn solve-captcha [captcha]
  (let [len (-> captcha count inc)]
    (->> captcha
         cycle
         (take len)
         (partition 2 1)
         (filter digits-match?)
         (map digit->int)
         (reduce +))))

(defn -main [input-filename]
  (-> input-filename
      slurp
      str/split-lines
      first
      solve-captcha
      println))
