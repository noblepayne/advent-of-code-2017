(ns advent-of-code-2017.day12.1
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.set :as set]))

(defn get-input
  "Build input map from file.
   e.g (w/ strings rather than keywords)
   {:0 #{:1 :2}
    :1 #{:0}
    :2 #{:0}"
  [filename]
  (->> filename
       slurp
       str/split-lines
       (map #(str/split % #" <-> "))
       (map #(vector (first %)
                     (set (str/split (second %) #", "))))
       (into {})))

(defn direct-link?
  "Is node contained in links?"
  [node links]
  (contains? links node))

(defn direct-links
  "Lookup node's links in the world"
  [node world]
  (get world node))

(defn path-to-node?
  "Is there a path from input-node to target-node in the world?"
  ([world target-node our-node] (path-to-node? world target-node our-node #{})) ;; initial seen set is empty
  ([world target-node our-node seen-set]
   (let [unseen-links (set/difference (direct-links our-node world)             ;; filter out seen links
                                      seen-set)]                                ;; to avoid loops
     (if (= our-node target-node)
       true                                                                     ;; always a path from self
       (if (direct-link? target-node unseen-links)
         true                                                                   ;; direct link?
         (some true?
               (map #(path-to-node? world                                       ;; is there a path through any of
                                    target-node                                 ;; our direct (unseen) links?
                                    %
                                    (conj seen-set our-node))
                    unseen-links)))))))


(defn find-linked-nodes
  ([world node] (find-linked-nodes world #{node} #{}))
  ([world nodes-to-process seen-set]
   (println nodes-to-process seen-set)
   (if (empty? nodes-to-process)
     seen-set
     (let [[f & r]      nodes-to-process
           links        (direct-links f world)
           to-process   (set/difference (set/union (set links)
                                                   (set r))
                                        seen-set)
           new-seen-set (conj seen-set f)]
       (recur world
              to-process
              new-seen-set)))))

(defn find-group
  "Find all nodes connected to target-node."
  [world target-node]
  (->> world
       keys
       (filter #(path-to-node? world target-node %))
       set))

(defn -main
  "How many nodes are connected to :0?"
  [input-filename]
  (-> input-filename
      get-input
      (find-group "0")
      count
      println))
