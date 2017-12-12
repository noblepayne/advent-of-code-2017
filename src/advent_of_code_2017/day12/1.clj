(ns advent-of-code-2017.day12.1)

(defn direct-link? [node node-set]
  (contains? node-set node))

(defn direct-links [node world]
  (get world node))

(defn path-to-node? [our-node target-node world seen-set]
  (let [dl (clojure.set/difference (direct-links our-node world) seen-set)]
    (if (= our-node target-node)
      true
      (if (direct-link? target-node dl)
        true
        (some true? 
              (map
               #(path-to-node? % target-node world (conj seen-set our-node)) dl))))))


(defn get-input [filename]
  (->> filename
      slurp
      clojure.string/split-lines
      (map #(clojure.string/split %
                                  #" <-> "))
      (map #(vector (first %) (clojure.string/split (second %) #", ")))
      (map #(vector (first %) (set (second %))))
      (into {})))

(defn test-fn [inp tar]
  (->> inp
       keys
       (map #(path-to-node? % tar inp #{}))
       (filter true?)
       count))

;;;;;;;;; part 2

(defn path-to-node-2? [our-node target-node world seen-set]
  (let [dl (clojure.set/difference (direct-links our-node world) seen-set)]
    (if (= our-node target-node)
      our-node
      (if (empty? dl)
        nil
        (if (direct-link? target-node dl)
          our-node
          (map #(path-to-node-2? % target-node world (conj seen-set our-node))
               dl))))))

(defn better-fn [inp tar]
  (->> inp
       keys
       (filter #(path-to-node? % tar inp #{}))
       set))

(defn solve-2 [world group-count]
  (let [ks (keys world)]
    (if (= 1 (count ks))
      (inc group-count)
      (let [k (first ks)
            g (better-fn world k)]
        (recur (reduce dissoc world g) (inc group-count))))))
