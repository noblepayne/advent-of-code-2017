(ns advent-of-code-2017.day24.1
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.pprint :refer [pprint]]))

(def magic-regex #"(\d+)/(\d+)")

(defn get-input [filename]
  (->> filename
      slurp
      str/split-lines
      (map (fn [line] (vec (map #(Integer. %) (drop 1 (re-matches magic-regex line))))))))


(defn find-starting-blocks [supplies]
  (filter (fn [x]
            ((complement empty?)
             (filter #(= 0 %)
                     x)))
          supplies))

;; [complete-seqs in-progress]
;; each in progress [seq supplies]

(comment
(defn new-inprogress [s sup options inprogress]
  (let [inprogress2 (drop 1 inprogress)
        inprogress3 (map (fn [op]
                           (let [new-sup (set/difference sup op)]
                             [(conj s op) new-sup]))
                         options)]
    (reduce conj inprogress2 inprogress3)))
)




(defn compat? [s supply]
  (let [ last2 (take-last 2 s)
        freq (frequencies (flatten last2))
        shared (first (first (filter (fn [[x y]] (not= 1 y)) freq)))
        free  (set/difference (set (last last2)) #{shared})
        rfree (if (and (empty? free) (= 1 (count (conj (set (last last2)) shared)))) ;; [[0 2] [2 2]] must accept [2 3]
                #{shared}
                free)]
    ((complement empty?)
     (filter rfree supply))))

(defn find-options [s sup]
  (filter #( compat? s %)
          sup))

(defn grow-solution [complete inprogress]
  ;(pprint inprogress)
                                        ;(Thread/sleep 2000)
 ;(println (count (set complete)))
  (if (empty? inprogress)
    complete
    (let [[s sup] (first inprogress)
          options (find-options s sup)]
      (if (empty? options)
        (recur (conj complete s) (drop 1 inprogress))
        (recur complete (reduce conj
                                (drop 1 inprogress)
                                (map (fn [option]
                                       (let [new-supplies (set/difference sup #{option})]
                                         [(conj s option) new-supplies]))
                                     options)))))))


(defn part1 [filename]
  (let [inp (set (get-input filename))
        starts (find-starting-blocks inp)
        inprogress
        (map (fn [start]
               [[[0 0] start] (set/difference inp #{start})])
             starts)]
    (grow-solution [] inprogress)))


(defn process-stuff [line]
  (reduce + (flatten line)))

(defn real-part1 [filename]
  (->> filename
       part1
       (map #(vector % (process-stuff %)))
       (sort-by second)
       last
       second))

(defn part2 [part1-output]
  (->> part1-output
       (map #(vector % (process-stuff %)))))
      ; (sort-by second #(> (count %1) (count %2)))
       ;last
       ;second))
