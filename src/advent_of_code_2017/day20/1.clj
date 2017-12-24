(ns advent-of-code-2017.day20.1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.repl :refer :all]))

(def pinput (io/resource "day20.txt"))

(def input-matcher #"(p|a|v)=<(-?\d+),(-?\d+),(-?\d+)>")

(defn get-input [filename]
  (->> filename
       slurp
       str/split-lines
       (map #(str/split % #", "))
       (map process-line)
       (map make-particle)))

(defn process-section [sect]
  (drop 1
        (re-matches input-matcher sect)))

(defn process-line [line]
  (map process-section
       line))

(defn make-particle [line]
  (reduce (fn [xs [x & r]]
            (assoc xs (keyword x) (vec (map #(Integer. %) r))))
          {}
          line))

(defn update-velocity [particle]
  (let [{:keys [:a :v :p]} particle
        new-v (vec (map + v a))]
    (assoc particle :v new-v)))

(defn update-position [particle]
  (let [{:keys [:a :v :p]} particle
        new-p (vec (map + p v))]
    (assoc particle :p new-p)))

(defn update-particle [particle]
  (-> particle
      update-velocity
      update-position))

(defn update-all [particles]
  (doall (map update-particle particles)))

(defn run-n-times [particles n]
  (nth (iterate update-all particles)
       n))

(defn distance [particle]
  (let [{:keys [:p]} particle]
    (reduce + (map #(Math/abs %) p))))

(defn min-distance [particles]
  (first (first (sort-by second
                        (map-indexed
                         #(vector %1 (distance %2))
                         particles)))))

(defn part-1 [filename]
  (-> filename
      get-input
      (run-n-times 1000)
      min-distance))

(defn find-any-collisions [particles]
  (->> particles
       (map :p)
       frequencies
       (filter (fn [[t c]] (not= c 1)))
       (map first)
       (#(if (empty? %) nil %))))

(defn filter-collisions [particles collisions]
  (vec
   (filter #((complement (set collisions)) (:p %)) particles)))

(defn purge-collisions [particles]
  (if-let [collisions (find-any-collisions particles)]
    (filter-collisions particles collisions)
    particles))

(defn run-tick [particles]
  (->> particles
       purge-collisions
update-all))

(defn run-n-times-2 [particles n]
  (nth (iterate run-tick particles)
       n))

(defn part-2 [filename]
  (-> filename
      get-input
      (run-n-times-2 1000)
      count))
