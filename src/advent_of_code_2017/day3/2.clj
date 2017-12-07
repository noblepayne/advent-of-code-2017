(ns advent-of-code-2017.day3.2)

(defn make-ring [s]
  (let [start (inc (* (- s 2) (- s 2)))
        end   (* s s)]
    (range start (inc end))))

(def odds
  (filter odd? (range)))

(defn generator []
  (mapcat #(->> %
                make-ring
                (partition (dec %)))
          odds))

(defn real-generator []
  (lazy-seq (cons '(1) (generator))))

