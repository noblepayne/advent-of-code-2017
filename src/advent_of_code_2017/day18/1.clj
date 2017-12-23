(ns advent-of-code-2017.day18.1
  (:require [clojure.repl :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.core.async :as async]))


(s/def ::reg string?)
(s/def ::val integer?)

(s/def ::set-op #(= "set" %))
(s/def ::snd-op #(= "snd" %))
(s/def ::add-op #(= "add" %))
(s/def ::mul-op #(= "mul" %))
(s/def ::mod-op #(= "mod" %))
(s/def ::rcv-op #(= "rcv" %))
(s/def ::jgz-op #(= "jgz" %))

(s/def ::val-or-reg (s/alt :val ::val :reg ::reg))

(s/def ::set (s/cat :op ::set-op :reg ::reg :val ::val-or-reg))
(s/def ::snd (s/cat :op ::snd-op :val ::val-or-reg))
(s/def ::add (s/cat :op ::add-op :reg ::reg :val ::val-or-reg))
(s/def ::mul (s/cat :op ::mul-op :reg ::reg :val ::val-or-reg))
(s/def ::mod (s/cat :op ::mod-op :reg ::reg :val ::val-or-reg))
(s/def ::rcv (s/cat :op ::rcv-op :val ::val-or-reg))
(s/def ::jgz (s/cat :op ::jgz-op :val1 ::val-or-reg :val2 ::val-or-reg))

(s/def ::inst (s/alt :set ::set :snd ::snd :add ::add :mul ::mul :mod ::mod :rcv ::rcv :jgz ::jgz))

(s/def ::world (s/cat :curpos integer?
                      :sounds vector?
                      :recovered vector?
                      :regs map?))

(def init-world [0 [] [] {}])

(defn get-val [[curpos sounds recovered regs] reg]
  (get regs reg 0))

(defn put-val [[curpos sounds recovered regs] reg val]
  [(inc curpos) sounds recovered (assoc regs reg val)])

(defn set-op [world reg val]
  (put-val world reg val))

(defn add-op [world reg val]
  (put-val world reg (+ val (get-val world reg))))

(defn mul-op [world reg val]
  (put-val world reg (* (get-val world reg) val)))

(defn mod-op [world reg val]
  (put-val world reg (mod (get-val world reg) val)))

(defn jgz-op [[curpos sounds recovered regs] val1 val2]
  (if (pos-int? val1)
    [(+ curpos val2) sounds recovered regs]
    [(inc curpos) sounds recovered regs]))

(defn snd-op [[curpos sounds recovered regs] val]
  [(inc curpos) (conj sounds val) recovered regs])

(defn rcv-op [[curpos sounds recovered regs] val]
  (let [new-r (if (not= 0 val) (conj recovered (peek sounds)) recovered)]
    [(inc curpos) sounds new-r regs]))

(defn get-val-or-reg [world val-or-reg]
  (let [[op val] val-or-reg]
    (if (= :reg op)
      (get-val world val)
      val)))

(defn run-inst [world [op inst]]
  (condp = op
    :set (set-op world (:reg inst) (get-val-or-reg world (:val inst)))
    :add (add-op world (:reg inst) (get-val-or-reg world (:val inst)))
    :mul (mul-op world (:reg inst) (get-val-or-reg world (:val inst)))
    :mod (mod-op world (:reg inst) (get-val-or-reg world (:val inst)))
    :jgz (jgz-op world (get-val-or-reg world (:val1 inst)) (get-val-or-reg world (:val2 inst)))
    :snd (snd-op world (get-val-or-reg world (:val inst)))
    :rcv (rcv-op world (get-val-or-reg world (:val inst)))))

(defn run-instructions
  ([instructions] (run-instructions init-world instructions))
  ([world instructions]
   ;;(println world)
   (let [current-inst (nth instructions (first world))
         new-world (run-inst world current-inst)
         inst-len (count instructions)]
     (if (not (empty? (nth new-world 2)))
       new-world
       (recur new-world instructions)))))

(defn make-int [strg]
  (try (Integer. strg)
       (catch NumberFormatException e strg)))

(defn get-input [filename]
  (->> filename
       slurp
       str/split-lines
       (map (fn [line]
              (let [[inst reg val] (str/split line #" ")]
                (if val
                  [inst (make-int reg) (make-int val)]
                  [inst (make-int reg)]))))
       (map #(s/conform ::inst %))))

(defn part1 [filename]
  (-> (run-instructions (get-input filename))
      (nth 2)
      first))
