(ns advent-of-code-2017.day6.1
  (:gen-class)
  (:require [clojure.string :as s]
            [clojure.pprint :refer [pprint]]))

(def world
  [{:bank 0 :val 0}
   {:bank 1 :val 2}
   {:bank 2 :val 7}
   {:bank 3 :val 0}])

(defn sort-fn [b1 b2]
  (if (= (:val b1) (:val b2))
    (compare (:bank b2) (:bank b1)) ;; the old switcheroo
    (compare (:val b1) (:val b2))))

(defn combine-results [freq world]
  (reduce (fn [world [bank num]]
            (let [cur-value (-> world (nth bank) :val)
                  new-value (+ cur-value num)]
              (assoc world bank {:bank bank :val new-value})))
          world
          freq
          ))

(defn distribute [world to-replace buffer]
  (let [freq (->> world (map :bank) cycle (drop (inc to-replace)) (take buffer) frequencies)]
   ;; (println "freq: " freq)
    (combine-results freq world)))

(defn get-world-order [world]
 (->> world (sort sort-fn) (map :bank) reverse))

(defn run-cycle [world-set current-world cycle-count]
  (let [new-cycle        (inc cycle-count)
        world-order      (get-world-order current-world)
        to-replace       (first world-order)
        replaced-world   (assoc current-world to-replace {:bank to-replace :val 0})
        buffer           (->> to-replace (nth current-world) :val)
        post-distribute  (distribute replaced-world to-replace buffer)
        new-world-set    (conj world-set post-distribute)]
    ;;(println world-order)
    ;;(println to-replace)
    ;;(println buffer)
    ;;(println (map :val post-distribute))
    ;;(println)
    ;;(println (count new-world-set))
    (if (= new-world-set world-set)
      [cycle-count post-distribute]
      (recur new-world-set post-distribute new-cycle))))

(def real-world
  [{:bank 0  :val 2}
   {:bank 1  :val 8}
   {:bank 2  :val 8}
   {:bank 3  :val 5}
   {:bank 4  :val 4}
   {:bank 5  :val 2}
   {:bank 6  :val 3}
   {:bank 7  :val 1}
   {:bank 8  :val 5}
   {:bank 9  :val 5}
   {:bank 10 :val 1}
   {:bank 11 :val 2}
   {:bank 12 :val 15}
   {:bank 13 :val 13}
   {:bank 14 :val 5}
   {:bank 15 :val 14}])


(defn part-two [world]
  (let [part-1 (run-cycle #{} world 1)
        new-world (second part-1)]
    (first
     (run-cycle #{new-world} new-world 1))))
