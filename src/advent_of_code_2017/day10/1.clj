(ns advent-of-code-2017.day10.1
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))


(s/def ::accum (s/cat ::curpos int?
                      ::skip   int?
                      ::numlist (s/coll-of number?)))

(def test-lenghts [3 4 1 5])
(def test-list (range 5))

(def real-lengths [83,0,193,1,254,237,187,40,88,27,2,255,149,29,42,100])
(def real-list (range 256))

(defn r2 [ac l]
  (let [[curpos skip numlist] ac
        len (count numlist)
        rest-s (- len l)
        rbase (drop curpos (cycle numlist))
        rsec (reverse (take l rbase))
        osec (take rest-s (drop l rbase))
        new-list (take len
                       (drop (- len curpos)
                             (cycle (concat rsec
                                            osec))))
        new-pos (mod (+ curpos l skip) len)
        new-skip (inc skip)]
    [new-pos new-skip new-list]))


(defn make-hash [seg-list length-list]
  (reduce r2
          (vector 0 0 seg-list)
          length-list))

(defn check-hash [output]
  (->> output
       last
       (take 2)
       (apply *)))
