(ns advent-of-code-2017.day25.1
  (:require [clojure.repl :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [clojure.java.io :as io]
            [clojure.string :as str]))


(defn get-input [filename]
  (->> filename
       slurp
       str/split-lines
       (partition-by #{""})
       (remove #{'("")})))

(defn process-input-1 [input]
  (->> input
       (re-matches #"Begin in state ([A-Z]).")
       second
       keyword))

(defn process-input-2 [input]
  (->> input
       (re-matches #"\D*(\d+)\D*")
       second
       Integer.))

(defn get-state [state-line]
  (->> state-line
       (re-matches #"In state ([A-Z]):")
       second
       keyword))

(defn get-current-value [value-line]
  (->> value-line
       (re-matches #"\D*(\d+)\D*")
       second
       Integer.))

(defn get-write-value [write-line]
  (->> write-line
       (re-matches #"\D*(\d+)\D*")
       second
       Integer.))

(defn get-movement [move-line]
  (->> move-line
       (re-matches #".*(right|left).*")
       second
       keyword))

(defn get-next-state [state-line]
  (->> state-line
       (re-matches #".*Continue with state ([A-Z]).")
       second
       keyword))

(defn process-value-group [vg]
  (let [[vl wl ml sl] vg]
    {(get-current-value vl)
     [(get-write-value wl)
      (get-movement ml)
      (get-next-state sl)]}))

(defn process-rule-group [rg]
  (let [[f & r] rg]
    {(get-state f)
     (reduce conj
             (map process-value-group (partition 4 r)))}))

(defn process-input [input]
  (let [[l1 l2]     (first input)
        start-state (process-input-1 l1)
        num-steps   (process-input-2 l2)]
    (reduce conj {:start start-state :steps num-steps}
            (map process-rule-group
                 (rest input)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn init-world [state]
  {:curpos 0
   :state state
   :tape {0 0}})

(def dirs
  {:left  -1
   :right  1})

(defn get-new-pos [direction curpos]
  (+ curpos
     (get dirs direction)))

(defn update-world-with-rule [world rule]
  (let [{:keys [:curpos :state :tape]} world
        [newval direction newstate]    rule
        newpos (get-new-pos direction curpos)]
    (as-> world <>
      (assoc <> :tape (assoc tape
                             curpos
                             newval))
      (assoc <> :state newstate)
      (assoc <> :curpos newpos))))


(defn take-step [rules world]
  (let [{:keys [:curpos :state :tape]} world
        curval (get tape curpos 0)
        rule (-> rules (get state) (get curval))
        new-world (update-world-with-rule world rule)]
    new-world))

(defn take-steps [rules world steps]
  (nth (iterate (partial take-step rules) world)
       steps))

(defn checksum [world]
  (count
   (remove (fn [[k v]] (zero? v))
           (:tape world))))

(defn part1 [inputfile]
  (let [rules (-> inputfile get-input process-input)
        steps (:steps rules)
        state (:start rules)
        world (init-world state)]
    (checksum
     (take-steps rules world steps))))
