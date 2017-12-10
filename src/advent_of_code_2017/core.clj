(ns advent-of-code-2017.core
  (:gen-class)
  (:require [clojure.pprint :refer [pprint]]
            [clojure.java.io :as io]
            [advent-of-code-2017.day1.1 :as day1-1]
            [advent-of-code-2017.day1.2 :as day1-2]
            [advent-of-code-2017.day2.1 :as day2-1]
            [advent-of-code-2017.day2.2 :as day2-2]
   ))

(def days
  {:1-1 ["Digital Captcha"  day1-1/-main]
   :1-2 ["Halfway Around"   day1-2/-main]
   :2-1 ["Corrupt Checksum" day2-1/-main]
   :2-2 ["Evenly Divisible" day2-2/-main]
   })

(defn -main
  [& args]
  (if args
    ((second (get days (read-string (first args)))) (io/resource (second args)))
    (do
      (println "Usage: lein run :2-3 day2.txt")
      (println "Available days: ")
      (pprint (into {}
                    (map
                     #(vector (first %) (first (second %)))
                     days))))))
