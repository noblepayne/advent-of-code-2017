(ns advent-of-code-2017.day10.2
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [advent-of-code-2017.day10.1 :as day10-1]))

(def pinput "83,0,193,1,254,237,187,40,88,27,2,255,149,29,42,100")

(defn string->list [string]
  (vec (map int string)))

(defn make-input [list]
  (conj list 17 31 73 47 23))

(defn run-seq [ac length]
  (reduce day10-1/r2
          ac
          length))

(defn run-sequences [li lengths]
  (let [inputs (repeat 64 lengths)]
    (reduce run-seq
            [0 0 li]
            inputs)))

(defn xorify [sparse-hash]
  (->> sparse-hash
       last
       (partition 16)
       (map #(apply bit-xor %))))


(defn make-hash [input-str]
  (->> input-str
       string->list
       make-input
       (run-sequences (range 256))
       xorify
       (map #(format "%02x" %))
       (apply str)))
