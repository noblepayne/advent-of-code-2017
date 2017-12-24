(ns advent-of-code-2017.day23.1
  (:require [clojure.repl :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.java.io :as io]))


(s/def ::reg string?)
(s/def ::val integer?)

(s/def ::val-or-reg (s/alt :val ::val :reg ::reg))

(s/def ::op #{"sub" "jnz" "mul" "set"})

(s/def ::inst (s/cat :op ::op :reg ::val-or-reg :val ::val-or-reg))
(s/def ::insts (s/coll-of ::inst))

(s/def ::world (s/cat :ran-instructions vector?
                      :curpos int?
                      :regs map?))

(defn make-world []
  (s/conform ::world [[] 0 {}]))

(defn set-world-pos
  "Set instruction pointer to pos"
  [world pos]
  (assoc world :curpos pos))

(defn set-reg-val
  "Set register value in world"
  [world reg val]
  (as-> world <>
    (:regs <>)
    (assoc <> reg val)
    (assoc world :regs <>)))

(defn get-reg-val
  "Gets register value in world, defaults to 0"
  [world reg]
  (get (:regs world) reg 0))

(defn get-val-or-reg
  "Converts register refs to values.
  Given [:reg \"a\"] returns that registers value
  Given [:val 2] returns 2"
  [world val-or-reg]
  (let [[op val] val-or-reg]
    (if (= :reg op)
      (get-reg-val world val)
      val)))

(defn inc-pos [world]
  (let [curpos (:curpos world)]
    (assoc world :curpos (inc curpos))))

(defn set-op [world reg val]
  (-> world
      (set-reg-val reg val)
      inc-pos))

(defn sub-op [world reg val]
  (let [cval (get-reg-val world reg)
        nval (- cval val)]
    (-> world
        (set-reg-val reg nval)
        inc-pos)))

(defn mul-op [world reg val]
  (let [cval (get-reg-val world reg)
        nval (* cval val)]
    (-> world
        (set-reg-val reg nval)
        inc-pos)))

(defn jnz-op [world reg val]
  (let [curpos (:curpos world)]
    (if (not= 0 reg)
      (set-world-pos world (+ curpos val))
      (inc-pos world))))

(defn run-inst [world {:keys [:op :reg :val]}]
  (let [rreg (second reg)
        rregval (get-val-or-reg world reg)
        rval (get-val-or-reg world val)
        rworld (assoc world :ran-instructions (conj (:ran-instructions world) op))]
    (condp = (keyword op)
      :set (set-op rworld rreg rval)
      :sub (sub-op rworld rreg rval)
      :mul (mul-op rworld rreg rval)
      :jnz (jnz-op rworld rregval rval))))

(defn run-instructions [world instructions]
  (let [inst-len (count instructions)
        valid-pos? #(< -1 % inst-len)]
    (loop [world world]
      (let [curpos (:curpos world)]
        (if (valid-pos? curpos)
          (let [inst (nth instructions curpos)
                new-world (run-inst world inst)]
            (println (:curpos new-world )(:regs new-world))
            (recur new-world))
          world)))))

(def pinput (io/resource "day23.txt"))

(defn make-int [strg]
  (try (Integer. strg)
       (catch NumberFormatException e strg)))

(defn get-input [filename]
  (->> filename
       slurp
       str/split-lines
       (map (fn [line]
              (let [[inst reg val] (str/split line #" ")]
                [inst (make-int reg) (make-int val)])))
       (s/conform ::insts)))

(defn part-1 [filename]
  (->> filename
       get-input
       (run-instructions (make-world))
       :ran-instructions
       frequencies
       :mul))

;;;;;;;


(defn make-world-2 []
  (s/conform ::world [[] 0 {"a" 1}]))

;; find range and step-size of outer index, thus the number of steps
;; note that the only command referencing h is gated by a jnz on f
;; f is set to 0 only once, gated by the condition b = d*e. If b is prime this can never be true.
;; For every non-prime b, there will be at least 1 combination of d and e s.t. de = b, and thus f will be set to 0.
;; at d=b (the final round of the d loop) we reach the f gate, since f=0 h is incremented.
;; the outer, b gate is controlled by a jump reached only when b = c, stepping b by its step size.
;; Count the number of non-prime b's and this will be the final sum of h.

(defn part-2 [c b step]
  (let [xf (comp (map (complement prime?))
                 (filter identity)
                 (map (fn [_] 1)))]
    (transduce xf
               +
               (range b (inc c) step))))
