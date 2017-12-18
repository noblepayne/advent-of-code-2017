(ns advent-of-code-2017.day18.1
  (:require [clojure.repl :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]))


(s/def ::reg string?)
(s/def ::val integer?)
(s/def ::op  string?)

(s/def ::inst (s/cat :op ::op :reg ::reg :val (s/alt :val ::val :reg ::reg)))
(s/def ::insts (s/coll-of ::inst))

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
                  [inst reg (make-int val)]
                  [inst reg]))))))
