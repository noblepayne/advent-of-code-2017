(ns advent-of-code-2017.day16.1
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.repl :refer :all]
            [clojure.java.io :as io]
            [clojure.data.finger-tree :refer [counted-double-list conjl]]))

(def t [\a \b \c \d \e])

(defn spin [ns n]
  (let [c (drop (- 16 n) (cycle ns))]
    (vec (take 16 c))))

(defn better-spin [ns n]
  (first (drop n (iterate #(conjl (pop %) (peek %)) ns))))

(defn swap-by-place [ns [p1 p2]]
  (let [p1-0 (nth ns p1)
        p2-0 (nth ns p2)]
    (assoc ns p1 p2-0 p2 p1-0)))

(defn swap-by-name2 [ns [n1 n2]]
  (-> ns
      (assoc (.indexOf ns n1) n2)
      (assoc (.indexOf ns n2) n1)))

(defn swap-by-name [ns [n1 n2]]
  (reduce
   (fn [[xs c] x]
     (conj
      xs
      (condp = x
          n1 n2
          n2 n1
          x)))
   []
   ns))

;; Parse input

(s/def ::s #(= % \s))
(s/def ::x #(= % \x))
(s/def ::p #(= % \p))
(s/def ::divider #(= % \/))
(s/def ::digit #{\1 \2 \3 \4 \5 \6 \7 \8 \9 \0})

(s/def ::spin
  (s/cat ::move ::s
         ::val1 (s/+ ::digit)))

(s/def ::byplace
  (s/cat ::move ::x
         ::val1 (s/+ ::digit)
         ::div  ::divider
         ::val2 (s/+ ::digit)))

(s/def ::byname
  (s/cat ::move ::p
         ::val1 char?
         ::div ::divider
         ::val2 char?))

(s/def ::move
  (s/alt ::spin ::spin
         ::byplace ::byplace
         ::byname ::byname))

(s/def ::moves
  (s/coll-of ::move))

(defn strseq->int [strseq]
  (Integer. (apply str strseq)))


(defn prep-move [move]
  (let [[spec m] move]
    (condp = spec
      ::spin    #(spin % (strseq->int (::val1 m)))
      ::byplace #(swap-by-place % [(strseq->int (::val1 m)) (strseq->int (::val2 m))])
      ::byname  #(swap-by-name2 % [(::val1 m) (::val2 m)])
      world)))

(defn get-input [file]
  (->> (-> file
           slurp
           str/split-lines
           first
           (str/split #","))
       (map vec)
       (s/conform ::moves)
       (map prep-move)))

(defn run-move [world move]
  (move world))

(defn dance [world moves]
  (reduce run-move world moves))


(def world [\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p])

(defn seq-counter 
  "calls callback after every n'th entry in sequence is evaluated. 
  Optionally takes another callback to call once the seq is fully evaluated."
  ([sequence n callback]
     (map #(do (if (= (rem %1 n) 0) (callback %1)) %2) (iterate inc 1) sequence))
  ([sequence n callback finished-callback]
     (drop-last (lazy-cat (seq-counter sequence n callback) 
                  (lazy-seq (cons (finished-callback) ())))))) 

(defn part-1 []
  (let [moves (get-input (io/resource "day16.txt"))]
    (dance world moves)))

(defn part-2 [init-world]
  (let [moves (get-input (io/resource "day16.txt"))]
    (reduce dance
            init-world
            (repeat 1000 moves))))

(defn find-period [world]
  (let [moves (get-input (io/resource "day16.txt"))]
    (inc (count
          (take-while (complement #{world})
                      (rest (iterate #(dance % moves)
                                     world)))))))
