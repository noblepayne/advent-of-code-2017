(ns advent-of-code-2017.day8.1
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]))

(def tinput
"b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10")

(s/def ::reg string?)
(s/def ::op string?)
(s/def ::val int?)
(s/def ::space #(= " " %))

(s/def ::form (s/cat :reg string? :op string? :val string?))

(defn process-input [input-string]
  (->> input-string
       str/split-lines
       (map #(str/split % #" if "))
       (map (fn [x]
              (map (fn [y]
                     (str/split y #" "))
                   x)))))

(defn conform [input]
  (map (fn [input-line]
         (map (fn [input-form]
                (s/conform ::form input-form))
              input-line))
       input))

(def != not=)

(def state (atom {}))

(defn check-register! [state reg]
  (if-not (contains? @state reg)
    (swap! state conj {reg 0})))

(defn test-con [state con]
  (let [val (Integer. (:val con))
        reg (keyword (:reg con))
        op  (symbol (:op con))]
    (check-register! state reg)
    ;(println op (reg @state) val)
    (eval (list
           op
           (reg @state)
           val))))

(def inc +)
(def dec -)

(defn run-op [state op]
  (let [reg (keyword (:reg op))
        oper  (symbol (:op op))
        val   (Integer. (:val op))]
    (check-register! state reg)
    (let [cur (reg @state)
          new (eval (list oper cur val))]
      ;(println "running " oper cur val)
      (swap! state assoc reg new))))

(defn process-line [state line]
  (let [op (first line)
        con (second line)]
    (if (test-con state con)
      (run-op state op)))
  @state)

(defn run-machine [state input]
  (->> input
       process-input
       conform
       (map (partial process-line state))))
       ;(mapcat vals)
       ;(apply max)))
  ;;(->> @state (sort-by second) reverse first second)
