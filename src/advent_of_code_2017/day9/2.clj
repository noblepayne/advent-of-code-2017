(ns advent-of-code-2017.day9.2
  (:require [clojure.core.match :refer [match]]
            [clojure.pprint :refer [pprint]]))

(defn get-new-state [new-char state]
  (match [new-char state]
         [_  {::erase? true  ::garbage? true           ;; erase one character
              ::depth _ ::count _ ::garbage-count _}]  (assoc state ::erase? false)

         [\! {::erase? false ::garbage? true           ;; turn on erase
              ::depth _ ::count _ ::garbage-count _}]  (assoc state ::erase? true)

         [\> {::erase? false ::garbage? true           ;; close garbage
              ::depth _ ::count _ ::garbage-count _}]  (assoc state ::garbage? false)

         [_  {::erase? false ::garbage? true           ;; increase garbage count
              ::depth _ ::count _ ::garbage-count gc}] (assoc state ::garbage-count (inc gc))

         [\< {::erase? false ::garbage? false          ;; start garbage
              ::depth _ ::count _ ::garbage-count _}]  (assoc state ::garbage? true)

         [\{ {::erase? false ::garbage? false          ;; new group
              ::depth d ::count _ ::garbage-count _}]  (assoc state ::depth (inc d))

         [\} {::erase? false ::garbage? false          ;; close group
              ::depth d ::count c ::garbage-count _}]  (assoc state ::depth (dec d)
                                                                    ::count (+ c d))
         ;; ignore other characters
         :else                                         state))

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
