(ns advent-of-code-2017.day9.2
  (:require [clojure.core.match :refer [match]]
            [clojure.pprint :refer [pprint]]))

(defn get-new-state [new-char state]
  (match [new-char state]
    [_  {::erase?   true       ;; erase one character
         ::garbage? true}]     (assoc state ::erase? false)

    [\! {::erase?   false      ;; turn on erase
         ::garbage? true}]     (assoc state ::erase? true)

    [\> {::erase?   false      ;; close garbage
         ::garbage? true}]     (assoc state ::garbage? false)

    [_  {::erase?   false
         ::garbage? true       ;; increase garbage count
         ::garbage-count gc}]  (assoc state ::garbage-count (inc gc))

    [\< {::erase?   false      ;; start garbage
         ::garbage? false}]    (assoc state ::garbage? true)

    [\{ {::erase?   false
         ::garbage? false      ;; new group
         ::depth    d}]        (assoc state ::depth (inc d))

    [\} {::erase?   false
         ::garbage? false
         ::depth    d          ;; close group
         ::count    c}]        (assoc state ::depth (dec d)
                                            ::count (+ c d))

                               ;; ignore other characters
    :else                      state))

(defn character-reducer [states new-char]
  (->> states
       last
       (get-new-state new-char)
       (conj states)))

(defn solve-puzzle [puzzle-input]
  (->> puzzle-input
       (reduce character-reducer
               [{::erase? false ::garbage? false
                 ::depth 0 ::count 0 ::garbage-count 0}])
       last
       pprint))
