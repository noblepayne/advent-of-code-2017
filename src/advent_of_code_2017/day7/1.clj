(ns advent-of-code-2017.day7.1
  (:require [clojure.string :as s]
            [clojure.pprint :refer [pprint]]
            [ubergraph.core :as uber]))

(def test-input-file-1 "/home/wes/src/clj/advent-of-code-2017/src/advent_of_code_2017/day7/test-input-1")
(def input-file-1 "/home/wes/src/clj/advent-of-code-2017/src/advent_of_code_2017/day7/test-input-2")


(defn process-input-line [line]
  (let [[name weight & [links]] (drop 1
                                      (re-matches #"(\w+) \((\d+)\) ?-?>? ?(.*)?" line))]
    (if (empty? links)
      [name weight nil]
      [name weight (s/split links #", ")])))

(defn process-input [input-file]
  (->> input-file
       slurp
       s/split-lines
       (map process-input-line)))

(defn entry->edges [entry]
  (map vector
       (repeat (first entry))
       (nth entry 2)))

(defn input->graph [input]
  (-> (uber/graph)
      (uber/add-directed-edges*
       (mapcat entry->edges
               input))))

(defn find-head-node [graph]
  (let [nodes (uber/nodes graph)
        in-degrees (map #(vector %
                                 (uber/in-degree graph %))
                        nodes)]
    (->> in-degrees
         (filter #(zero? (second %)))
         first
         first)))

(-> input-file-1
    process-input
    input->graph
    find-head-node)


;;; part 2


(defn input->weightmap [input]
  (reduce
   (fn [xs [n w & l]]
     (assoc xs n (Integer. w)))
   {}
   input))


(defn calc-node-weight [graph node weight-map]
  (let [suc (uber/successors graph node)
        weight (get weight-map node)]
    (if (empty? suc)
      weight
      (+ weight (reduce +
                        (map #(calc-node-weight graph % weight-map)
                             suc))))))

(defn balanced? [graph node weight-map]
  (let [suc (uber/successors graph node)]
    (if (empty? suc)
      true
      (apply = (map #(calc-node-weight graph % weight-map) suc)))))

(defn find-imbalanced-node [graph weight-map]
  (->> (uber/nodes graph)
       (map #(vector % (balanced? graph % weight-map)))
       (filter (comp not second))
       ;;first
       ;;first
       ))

(defn candidates [graph weight-map]
  (->> (uber/nodes graph)
       (map #(vector % (balanced? graph % weight-map)))
       (filter (comp not second))))

(defn better-find-imbalanced-node [graph weight-map]
  (let [candidates (candidates graph weight-map)
        cand-set (into #{} (map first candidates))
        child-set (into #{}
                        (mapcat #(uber/predecessors graph %)
                                cand-set))]
    (first (clojure.set/difference cand-set child-set))))

(defn new-node-weight [graph node weight-map]
  (let [suc (uber/successors graph node)
        w   (map #(vector %
                          (calc-node-weight graph % weight-map))
                 suc)
        weights (map second w)
        ws      (set weights)
        fr      (frequencies weights)
        odd-freq (apply min (vals fr))
        odd-total-weight (->> fr
                              (filter #(= odd-freq (second %)))
                              first
                              first)
        odd-node (first (first (filter #(= odd-total-weight (second %))
                         w)))
        odd-weight (get weight-map odd-node)
        diff (- (first (clojure.set/difference ws #{odd-total-weight})) odd-total-weight)]
    (+ odd-weight diff)))


(defn child-weights [graph node weight-map]
  (map #(vector % (calc-node-weight graph % weight-map)) (uber/successors graph node)))



(let [input (process-input input-file-1)
      wm    (input->weightmap input)
      graph (input->graph input)]
  (new-node-weight graph (better-find-imbalanced-node graph wm) wm))
