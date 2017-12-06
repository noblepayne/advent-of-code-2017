(ns advent-of-code-2017.day4.1
  (:gen-class))


(def stack1 [0 3 0 1 -3])


(defn reduce-stack [[steps place stack]]
  (let [cur-value (nth stack place)
        new-place (+ place cur-value)]
    (if (or (>= new-place (count stack))
            (> 0 new-place))
      steps
      (let [new-value (if (>= cur-value 3) (dec cur-value) (inc cur-value))
            new-stack (assoc stack place new-value)]
        (recur [(inc steps) new-place new-stack])))))
