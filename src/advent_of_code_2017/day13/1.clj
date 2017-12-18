(ns advent-of-code-2017.day13.1
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.repl :refer :all]
            [criterium.core :as crit]))

(defn get-input [input-file]
  (->> input-file
      slurp
      str/split-lines
      (map #(str/split % #": "))
      (map (fn [x] (map #(Integer. %) x)))
      (map vec)
      (into {})))


(defn scanner-position [world depth time]
  (let [r (get world depth)
        rang (range r)
        back (reverse (drop 1 (drop-last 1 rang)))
        sseq  (cycle (concat rang back))]
    (nth sseq time)))


(defn find-catches [world]
  (->> world
       keys
       sort
       (map #(vector % (scanner-position world % %)))
       (filter #(= (second %) 0))))


(defn calc-severity [world catches]
  (->> catches
       (map first)
       (reduce (fn [sum d]
                 (let [r (get world d)
                       p (* d r)]
                   (+ p sum)))
               0)))

(defn solve-puzzle [filename]
  (let [world (get-input filename)
        catches (find-catches world)]
    (calc-severity world catches)))

(defn get-firewall-length [range]
  (- (* 2 range) 2))

(defn better-scanner-position [world depth time]
  (let [r (get world depth)]
    (mod time (get-firewall-length r))))

(defn find-catches-delayed [world delay]
  (->> world
       keys
       sort
       (map #(vector % (better-scanner-position world % (+ % delay))))
       (filter #(= (second %) 0))))


(defn solve-2 [world]
  (->> (range)
       (map #(vector % (find-catches-delayed world %)))
       (drop-while #(not (empty? (second %))))
       first
       first))


(defn better-fcd [world sorted-world-keys delay]
  (let [t (comp (map #(vector % (better-scanner-position world % (+ % delay))))
                (filter #(= (second %) 0)))]
    (transduce t conj sorted-world-keys)))

(defn better-solve [world]
  (let [swk (sort (keys world))
        t (comp (map #(vector % (better-fcd world swk %)))
                (filter #(empty? (second %)))
                (take 10))]
    (transduce t concat (range))))
