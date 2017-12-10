(ns advent-of-code-2017.day9.2
  (:require [clojure.core.match :refer [match]]))

(defn get-new-state [state new-char]
  (match [new-char state]
    [_  {:erase? true :garbage? true}]  (assoc state :erase? false)    ;; erase char, flip erase
    [\! {:garbage? true}]               (assoc state :erase? true)     ;; set erase
    [\> {:garbage? true}]               (assoc state :garbage? false)  ;; close garbage
    [_  {:garbage? true :gcount gc}]    (assoc state :gcount (inc gc)) ;; increase garbage count
    [\< {:garbage? false}]              (assoc state :garbage? true)   ;; start garbage
    [\{ {:depth d}]                     (assoc state :depth (inc d))   ;; new group
    [\} {:depth d :count c}]            (assoc state :depth (dec d)    ;; close group
                                                     :count (+ c d))
    :else                               state))                        ;; ignore other characters

(defn solve-puzzle [puzzle-input]
  (reduce get-new-state
          {:erase? false :garbage? false :depth 0 :count 0 :gcount 0}  ;; initial state
          puzzle-input))

(comment
  (solve-puzzle (slurp "/tmp/input")))
