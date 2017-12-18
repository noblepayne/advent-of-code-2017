(ns advent-of-code-2017.day15.1
  (:require [clojure.string :as str]
            [clojure.repl :refer :all]))

(def magic-num 2147483647)

(defn genr [factor n]
  (rem (* factor n) magic-num))

(defn make-generator [factor]
  (partial genr factor))

(defn log-2 [n]
  (/ (Math/log n) (Math/log 2)))

(defn bitlen [n]
  (Math/ceil (log-2 n)))

(defn lower-16 [n]
  (let [bn (bitlen n)]
    (reduce #(bit-clear %1 %2) n (range 16 bn))))

(defn judge [[n1 n2]]
  (= (lower-16 n1) (lower-16 n2)))

(defn problem-1 [g1 g2 n1 n2]
  (let [ga (make-generator g1)
        gb (make-generator g2)
        g-stream (map vector (iterate ga n1) (iterate gb n2))
        xf (comp (take 4e7) (filter judge))]
    (->> (transduce xf conj g-stream)
         count)))

(defn problem-2 [g1 g2 f1 f2 n1 n2]
  (let [ga (make-generator g1)
        gb (make-generator g2)
        gstream (map vector
                     (filter #(= 0 (mod % f1)) (iterate ga n1))
                     (filter #(= 0 (mod % f2)) (iterate gb n2)))
        xf (comp (take 5e6)
                 (filter judge))]
    (->> (transduce xf conj gstream)
         count)))
