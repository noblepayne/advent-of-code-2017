(ns advent-of-code-2017.day14.1
  (:require [clojure.spec.alpha :as spec]
            [clojure.string :as str]
            [clojure.repl :refer :all]
            [advent-of-code-2017.day10.2 :as day10-2]
            [clojure.set :as set]))

(defn stupid-binary [i]
  (as-> i <>
       (Integer/toBinaryString <>)
       (format "%8s" <>)
       (str/replace <> " " "0")))

(defn new-make-hash [input-str]
  (->> input-str
       day10-2/string->list
       day10-2/make-input
       (day10-2/run-sequences (range 256))
       day10-2/xorify
       (map stupid-binary)))

(defn binary-row [row-string]
  (->> row-string
       new-make-hash
       (mapcat vec)
       vec))

(defn make-all-rows [input-str]
  (map #(binary-row (str input-str "-" %))
       (range 128)))

(defn used-filter [row]
  (->> row
       (filter #(= \1 %))
       count))

(defn solve-puzzle [puzz-input]
  (reduce +
          (map used-filter
               (make-all-rows puzz-input))))

;;;;;;;;;;;;;
; [row col] ;; pos


(defn labeled-world [world]
  (let [xf (map-indexed (fn [i row]
                          (map-indexed #(vector i %1 (Integer. (str %2))) row)))]
    (transduce xf conj world)))


(defn get-cell [world pos]
  (let [[r c] pos]
    (-> world
        (nth r)
        (nth c))))

(defn find-first-used-space [world start]
  (let [[r c] start
        damt (+ c (* 128 r))]
    (->> world
         flatten
         (partition 3)
         (drop damt)
         (drop-while #(= 0 (nth % 2)))
         first
         (take 2)
         vec)))

(defn get-neighbors [position-vec]
  (let [[row column] position-vec
        neighbors    [          [(dec row) column]
                       [row (dec column)] [row (inc column)]
                                [(inc row) column]
                     ]
        ]
    (filter (fn [neighbor]
              (every? #(<= 0 % 127) neighbor))
            neighbors)))

(defn find-friends
  ([world pos] (find-friends world [pos] #{pos}))
  ([world poss friends]
   (let [[p & re] poss
         [r c]    p
         n (get-neighbors p)
         cells (map (partial get-cell world) n)
         good-neigh (filter #(= 1 (nth % 2)) cells)
         new-poss (set (map (comp vec (partial take 2)) good-neigh))
         real-new-pos (clojure.set/difference new-poss friends)
         real-real-new-pos (concat real-new-pos re)]
     (if (empty? real-real-new-pos)
       friends
       (recur world real-real-new-pos (clojure.set/union friends real-new-pos))))))

(defn negate [world friend]
  (let [[r c] friend
        row (vec (nth world r))
        n-row (assoc row c [r c 0])]
    (assoc world r n-row)))

(defn remove-friends [world friends]
  (reduce negate
          world
          friends))

(defn solve-this-thing [world count last-pos]
  (println count)
  (let [start (find-first-used-space world last-pos)]
    (println start)
    (if (empty? start)
      count
      (let [friends (find-friends world start)
            new-world (remove-friends world friends)]
        (recur new-world (inc count) start)))))
