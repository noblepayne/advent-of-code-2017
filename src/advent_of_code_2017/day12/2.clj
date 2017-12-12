(ns advent-of-code-2017.day12.2
  (:require [advent-of-code-2017.day12.1 :as day12-1]))

(defn count-all-groups
  "Count all groups (sets of distinct interconnected nodes) in world."
  ([world] (count-all-groups world 0))
  ([world group-count]
   (let [nodes (keys world)]
     (if (= 1 (count nodes))
       (inc group-count)                    ;; one node is a group of 1
       (let [node  (first nodes)            ;; grab a node
             group (day12-1/find-group world node)]
         (recur (reduce dissoc world group) ;; remove all group nodes from world
                (inc group-count)))))))     ;; and increase group count


(defn -main
  "How many groups exist in world?"
  [input-filename]
  (-> input-filename
      day12-1/get-input
      count-all-groups
      println))
