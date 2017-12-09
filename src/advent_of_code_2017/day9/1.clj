(ns advent-of-code-2017.day9.1)

(defn reducer [xs x]
  (let [data (last xs)
        [erase? garbage? cur-group group-count] data]
    (if erase?
      (conj xs [false garbage? cur-group group-count])
      (if (= x \!)
        (conj xs [true garbage? cur-group group-count])
        (if garbage?
          (if (= x \>)
            (conj xs [false false cur-group group-count])
            (conj xs data))
          (condp = x
            \<  (conj xs [false true cur-group group-count])
            \{  (conj xs [false false (inc cur-group) group-count])
            \}  (conj xs [false false (dec cur-group) (+ group-count cur-group)])
            (conj xs data)))))))

(def input "")

(last
      (last
            (reduce reducer
                    [[false false 0 0]]
                    input)))
