(ns advent-of-code-2017.day19.1
  (:require [clojure.string :as str]
            [clojure.repl :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as set]))

(defn get-input [filename]
  (-> filename
      slurp
      str/split-lines))

(comment
  [pos dir seen]
  pos is [r c]
  dir is #{:l :r :u :d})

(def dirs
  {:l [0 -1]
   :r [0 1]
   :u [-1 0]
   :d [1 0]})

(def op-dir
  {:l :r
   :r :l
   :u :d
   :d :u})

(def dirset
  (set (map first dirs)))

(defn take-step [r c d]
  (vec (map +
            [r c]
            (get dirs d))))

(defn get-start [world]
  [[0 (.indexOf (first world) "|")] :d [] 0])

(defn get-char [world r c]
  (try
    (-> world
        (nth r)
        (nth c))
    (catch IndexOutOfBoundsException e \space)))

(defn proc-pipe [world [[r c] d seen steps]]
  [(take-step r c d) d seen (inc steps)])

(defn proc-dash [world [[r c] d seen steps]]
  [(take-step r c d) d seen (inc steps)])

(defn proc-corner [world [[r c] d seen steps]]
  (let [dirs-to-explore (set/difference dirset #{(get op-dir d)})
        places (map #(vector % (take-step r c %)) dirs-to-explore)
        [newd newpos] (first (filter (fn [[d [r c]]]
                                       (not= \space
                                             (get-char world r c)))
                                     places))]
    [newpos newd seen (inc steps)]))

(defn proc-letter [world [[r c] d seen steps] ch]
  [(take-step r c d) d (conj seen ch) (inc steps)])


(defn gspacemet-next [world curpos]
  (let [[[r c] d seen steps] curpos
        ch (get-char world r c)]
    (condp = ch
      \| (proc-pipe world curpos)
      \- (proc-dash world curpos)
      \+ (proc-corner world curpos)
      \space curpos
      (proc-letter world curpos ch))))

(defn walk-path
  ([world] (walk-path world (get-start world)))
  ([world curpos]
   (let [newpos (get-next world curpos)]
     (if (= curpos newpos)
       curpos
       (recur world newpos)))))

(defn part1 [world]
  (apply str
         (-> world
             walk-path
             (nth 2))))

(defn part2 [world]
  (-> world
      walk-path
      last))
