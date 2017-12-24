(ns advent-of-code-2017.day22.1
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(def dirorder [:u :r :d :l])
(def dirs {:u [-1 0]
           :d [1 0]
           :l [0 -1]
           :r [0 1]})

(def states #{:infected :clean})

(s/def ::pos (s/coll-of int?))
(s/def ::dir #(contains? dirs %))
(s/def ::map map?) ;; keys are pos', values are in states

(s/def ::world (s/cat :pos ::pos
                      :dir ::dir
                      :map ::map
                      :infection-count int?))

(defn magic-range [s]
  (let [b (int (Math/floor (/ s 2)))]
    (range (- b) (inc b))))

(defn get-input [filename]
  (->> filename
       slurp
       str/split-lines
       (map seq)))

(defn make-more-magic-range [s]
  (for [r (magic-range s)
        c (magic-range s)]
    [r c]))

(defn final-input [filename]
  (let [inp (get-input filename)
        s (count inp)]
    (zipmap (make-more-magic-range s) (map #(get {\. :clean \# :infected} %) (flatten inp)))))

(defn infected? [world pos]
  (= :infected
     (get (:map world) pos)))

(defn turn [world toturn]
  (let [{:keys [:dir]} world
        orderpos (.indexOf dirorder dir)
        neworderpos (mod (+ toturn orderpos) 4)
        newdir (get dirorder neworderpos)]
    (assoc world :dir newdir)))

(defn turn-right [world]
  (turn world 1))

(defn turn-left [world]
  (turn world -1))

(defn update-world [world state]
  (let [{:keys [:pos :map]} world]
        (assoc world
               :map
               (assoc map pos state))))

(defn clean [world]
  (update-world world :clean))
(defn infect [world]
  (-> world
      (update-world :infected)
      (assoc :infection-count (inc (:infection-count world)))))

(defn move [world]
  (let [{:keys [:pos :dir]} world
        newpos (map + (get dirs dir) pos)]
    (assoc world :pos newpos)))

(defn make-world [filename]
  (let [inp (final-input filename)]
    (s/conform ::world [[0 0] :u inp 0])))

(defn pick-new-dir [world]
  (let [{:keys [:pos :map]} world]
    (if (infected? world pos)
      (turn-right world)
      (turn-left world))))

(defn infect-or-not [world]
  (let [{:keys [:pos :map]} world]
    (if (infected? world pos)
      (clean world)
      (infect world))))

(defn run-burst [world]
  (->> world
       pick-new-dir
       infect-or-not
       move))

(defn nth-burst [world n]
  (nth (iterate run-burst world)
       n))

(defn part1 [filename]
  (-> (make-world filename)
      (nth-burst 10000)
      :infection-count))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def new-states #{:infected :clean :weakened :flagged})

(defn get-new-state [current-state]
  (get {:infected :flagged
        :flagged :clean
        :clean :weakened
        :weakened :infected}
       current-state))

(defn update-world-state [world]
  (let [{:keys [:pos :map]} world
        curstate (get map pos :clean)
        new-state (get-new-state curstate)
        newmap (assoc map pos new-state)
        newworld (assoc world :map newmap)]
    (if (= new-state :infected)
      (assoc newworld :infection-count (inc (:infection-count newworld)))
      newworld)))

(defn pick-new-dir-2 [world]
  (let [{:keys [:pos :map :dir]} world
        curnode (get map pos :clean)]
    (turn world
          (condp = curnode
            :clean -1
            :weakened 0
            :infected 1
            :flagged 2))))

(defn run-burst-2 [world]
  (->> world
       pick-new-dir-2
       update-world-state
       move))

(defn nth-burst-2 [world n]
  (nth (iterate run-burst-2 world)
       n))

(defn part2 [filename]
  (-> (make-world filename)
      (nth-burst-2 10000000)
      :infection-count))
