(ns advent-of-code-2017.day9.1
  (:require [clojure.core.match :refer [match]]
             [clojure.spec.alpha :as s]))

(defn reducer [xs x]
  (let [data (last xs)
        [erase? garbage? cur-group group-count garbage-count] data]
    (if erase?
      (conj xs [false garbage? cur-group group-count garbage-count])
      (if (= x \!)
        (conj xs [true garbage? cur-group group-count garbage-count])
        (if garbage?
          (if (= x \>)
            (conj xs [false false cur-group group-count garbage-count])
            (conj xs [erase? garbage? cur-group group-count (inc garbage-count)]))
          (condp = x
            \<  (conj xs [false true cur-group group-count garbage-count])
            \{  (conj xs [false false (inc cur-group) group-count garbage-count])
            \}  (conj xs [false false (dec cur-group) (+ group-count cur-group) garbage-count])
            (conj xs data)))))))



(s/def ::accumulator (s/cat ::erase? boolean?
                            ::garbage? boolean?
                            ::depth int?
                            ::count int?
                            ::garbage-count int?))

(defn better-reducer [xs x]
  (let [state (s/conform ::accumulator (last xs))]
    (->>
     (match [x state]
            [_  {::erase? true  ::garbage? _     ::depth _ ::count _ ::garbage-count _}]  (assoc state ::erase? false)
            [\! {::erase? false ::garbage? _     ::depth _ ::count _ ::garbage-count _}]  (assoc state ::erase? true)
            [\> {::erase? false ::garbage? true  ::depth _ ::count _ ::garbage-count _}]  (assoc state ::garbage? false)
            [_  {::erase? false ::garbage? true  ::depth _ ::count _ ::garbage-count gc}] (assoc state ::garbage-count (inc gc))
            [\< {::erase? false ::garbage? false ::depth _ ::count _ ::garbage-count _}]  (assoc state ::garbage? true)
            [\{ {::erase? false ::garbage? false ::depth d ::count _ ::garbage-count _}]  (assoc state ::depth (inc d))
            [\} {::erase? false ::garbage? false ::depth d ::count c ::garbage-count _}]  (assoc state
                                                                                                 ::depth (dec d)
                                                                                                 ::count (+ d c))
            :else                                                                         state)
     (s/unform ::accumulator)
     (conj xs))))

(defn solve-puzzle [puzzle-input]
  (->> puzzle-input
       (reduce better-reducer [[false false 0 0 0]])
       last))

(def input "")

(last
      (last
            (reduce reducer
                    [[false false 0 0 0]]
                    input)))
