(ns advent-of-code-2017.day3.2)

(defn make-ring [s]
  (let [start (inc (* (- s 2) (- s 2)))
        end   (* s s)]
    (range start (inc end))))

(defn find-row [n]
  (let [root (int (Math/ceil (Math/sqrt n)))]
    (if (even? root)
        (inc root)
        root)))

(def odds
  (filter odd? (range)))

(defn generator []
  (mapcat #(->> %
                make-ring
                (partition (dec %)))
          odds))

(defn real-generator []
  (lazy-seq (cons '(1) (generator))))


(defn index->rpo [index]
  (let [r (find-row index)
        ps (dec r)
        base-r (dec ps)
        base (inc (* base-r base-r))
        diff (- index base)
        part (int (/ diff ps))
        part-base (* ps part)
        offset (- diff part-base)]
    [r part offset]))


(defn rpo->index [[r p o]]
  (let [real-r (- r 2)
        ring-base (inc (* real-r real-r))
        part-size (dec r)
        part-base (* p part-size)
        index (+ ring-base
                 (+ part-base o))]
    index))

(defn deal-with-start [xs i]
  (if (> (count xs) i)
    (nth xs i)
    nil))


(def testing 266330)

(defn get-value [xs i]
  (get xs (dec i)))

(def func-map
  {:1 (fn [xs i]
        (let [[r p o] (index->rpo i)
              p1-i (dec i)
              p1 (get-value xs p1-i)
              p2-i (rpo->index [(dec (dec r)) p o])
              p2 (get-value xs p2-i)]
          (+ p1 p2)))

   :2 (fn [xs i]
        (let [[r p o] (index->rpo i)
              p1-i (dec i)
              p2-i (dec p1-i)
              p3-i (rpo->index [(dec (dec r)) p o])
              p4-i (dec p3-i)
              p1 (get-value xs p1-i)
              p2 (get-value xs p2-i)
              p3 (get-value xs p3-i)
              p4 (get-value xs p4-i)]
          (+ p1 p2 p3 p4)))

   :3 (fn [xs i]
        (let [[r p o] (index->rpo i)
              p1-i (dec i)
              new-r (dec (dec r))
              p2-i (rpo->index [new-r p o])
              p3-i (dec p2-i)
              p4-i (dec p3-i)
              p1 (get-value xs p1-i)
              p2 (get-value xs p2-i)
              p3 (get-value xs p3-i)
              p4 (get-value xs p4-i)]
          (+ p1 p2 p3 p4)))

   :4 (fn [xs i]
        (let [[r p o] (index->rpo i)
              p1-i (dec i)
              new-r (dec (dec r))
              p2-i (rpo->index [new-r p (dec o)])
              p3-i (dec p2-i)
              p1 (get-value xs p1-i)
              p2 (get-value xs p2-i)
              p3 (get-value xs p3-i)]
          (+ p1 p2 p3)))


   :5 (fn [xs i]
         (let [[r p o] (index->rpo i)
              p1-i (dec i)
              p2-i (rpo->index [(dec (dec r)) p (dec (dec o))])
              p1 (get-value xs p1-i)
              p2 (get-value xs p2-i)]
           (+ p1 p2)))})


(defn process-thing [start thing]
  (reduce (fn [xs [k i]]
            (conj xs ((k func-map) xs i)))
            start
          thing))

