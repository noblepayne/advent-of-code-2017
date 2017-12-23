(ns advent-of-code-2017.day18.2
  (:require [clojure.repl :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.core.async :as async :refer [<!! >!! chan close! timeout alts!! thread]]
            [clojure.java.io :as io]
            [advent-of-code-2017.day18.1 :as d18 :refer [get-input]]))

(s/def ::chan #(instance? clojure.core.async.impl.channels.ManyToManyChannel %))

;; model a place to keep state as we step through instructions
(s/def ::world (s/cat :curpos integer?  ;; current position
                      :sndcnt integer?  ;; send count
                      :in-chan ::chan   ;; input (rcv) channel
                      :out-chan ::chan  ;; output (snd) channel
                      :step-chan ::chan ;; step (progress) channel
                      :regs map?))      ;; registor value store

(defn make-world
  "Return new world with specified channels and program value.
  n.b. returns conformed value"
  [world-num inchan outchan stepchan]
  (s/conform ::world
             [0 0 inchan outchan stepchan {"p" world-num}]))

(defn inc-world-val
  "Increments a value stored in the world, e.g. curpos"
  [world key]
  (let [curpos (get world key)]
    (assoc world key (inc curpos))))

(defn inc-pos
  "Increments current instruction pointer (position)"
  [world]
  (inc-world-val world :curpos))

(defn inc-sndcnt
  "Increments send count"
  [world]
  (inc-world-val world :sndcnt))

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


(defmulti run-inst
  "Multimethod to run an instruction.
  Given world and inst returns new world.
  Dispatches on instruction keyword. e.g.
    [:snd {:op \"snd\", :val [:reg \"a\"]}]
  dispatches on :snd"
  #(-> %& second first))
(defmethod run-inst
  :snd
  [world inst]
  (let [{:keys [:val]} (second inst)
        rval (get-val-or-reg world val)]            ;; get referenced value
    (if-let [sent? (>!! (:out-chan world) rval)]    ;; attempt to send on output channel
      (-> world
          inc-sndcnt                                ;; increase sent count
          inc-pos)                                  ;; move to next inst
      world)))
(defmethod run-inst
  :rcv
  [world inst]
  (let [{:keys [:val]} (second inst)
        reg (second val)]
    (if-let [recv-val (<!! (:in-chan world))]       ;; attempt to receive from input channel
      (-> world
          (set-reg-val reg recv-val)                ;; update world with received value
          inc-pos)
      world)))
(defmethod run-inst
  :set
  [world inst]
  (let [{:keys [:reg :val]} (second inst)
        rval (get-val-or-reg world val)]
    (-> world
        (set-reg-val reg rval)                      ;; Set register to refd value
        inc-pos)))
(defmethod run-inst
  :add
  [world inst]
  (let [{:keys [:reg :val]} (second inst)
        rval (get-val-or-reg world val)
        cval (get-reg-val world reg)]
    (-> world
        (set-reg-val reg (+ rval cval))             ;; add and set
        inc-pos)))
(defmethod run-inst
  :mul
  [world inst]
  (let [{:keys [:reg :val]} (second inst)
        rval (get-val-or-reg world val)
        cval (get-reg-val world reg)]
    (-> world
        (set-reg-val reg (* rval cval))             ;; mul and set
        inc-pos)))
(defmethod run-inst
  :mod
  [world inst]
  (let [{:keys [:reg :val]} (second inst)
        rval (get-val-or-reg world val)
        cval (get-reg-val world reg)]
    (-> world
        (set-reg-val reg (mod cval rval))           ;; mod and set
        inc-pos)))
(defmethod run-inst
  :jgz
  [world inst]
  (let [{:keys [:val1 :val2]} (second inst)
        rval1 (get-val-or-reg world val1)
        rval2 (get-val-or-reg world val2)
        curpos (:curpos world)]
    (if (pos-int? rval1)
      (set-world-pos world (+ curpos rval2))        ;; jump by val2 if val1 is positive int
      (inc-pos world))))


(defn run-instructions
  "Given init world run all instructions and return final resulting world."
  [world instructions]
  (let [inst-len (count instructions)
        valid-pos? #(< -1 % inst-len)]              ;; inst must be within instructions
    (loop [world world]
      (let [curpos (:curpos world)]
        (if (and (valid-pos? curpos)                ;; Only continue if both valid inst
                 (>!! (:step-chan world) 1))        ;; and we can take a step (will return false when step-chan is closed)
          (let [inst (nth instructions curpos)
                new-world (run-inst world inst)]    ;; compute new world
            (recur new-world))                      ;; and run next instruction
          world)))))                                ;; otherwise finish and return world

(defn part-2
  "Run two communicating programs until finish or deadlock.
  Given input filename return final computed worlds."
  [inputname]
  (let [input (get-input inputname)                                 ;; conformed input
        chan0 (chan 10000)                                          ;; snd-rcv chan 0
        chan1 (chan 10000)                                          ;; snd-rcv chan 1
        step-chan (chan 10000)                                      ;; shared step/progress chan
        world0 (make-world 0 chan0 chan1 step-chan)                 ;; p0's world
        world1 (make-world 1 chan1 chan0 step-chan)                 ;; p1's world (w/ chan's switched)
        outchan0 (async/thread (run-instructions world0 input))     ;; channel to rcv thread's results
        outchan1 (async/thread (run-instructions world1 input))]
    (while
        (first (alts!! [step-chan                                   ;; Take from progress chan w/ 1s timeout. While receiving
                        (timeout 1000)])))                          ;; progress under this timeout continue taking steps
    (close! chan0)                                                  ;; otherwise close all channels
    (close! chan1)
    (close! step-chan)
    (doall
     (map #(select-keys % [:curpos :sndcnt :regs])
          [(<!! outchan0) (<!! outchan1)]))))                       ;; and return world results computed on threads
