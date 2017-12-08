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

(comment
147  142  133  122   59
304    5    4    2   57
330   10    1    1   54
351   11   23   25   26
362  747  806--->   ...

         start
         (i-1) + (r-2, p, n)

         second
         (i-1) + (r-2, p, n) + (r-2, p, n-1) + (i-2)

         middle
         (r-2, p, n) + (r-2, p, n-1) + (r-2, p, n-2) + (i-1)

         second-to-last
         (i-1) + (r-2, p, n-1) + (r-2, p, n-2)

         last
         (i-1) + (r-2, p, n-2)

)


(defn deal-with-start [xs i]
  (if (> (count xs) i)
    (nth xs i)
    nil))

(defn start-space [xs i]
  (let [[r p o] (index->rpo i)
        piece-1 (nth xs (dec i))
        piece-2-i (rpo->index [(- r 2) p o])
        piece-2 (nth xs piece-2-i)]
    (+ piece-1 piece-2)))

(defn second-space [xs i]
  (let [[r p o] (index->rpo i)
        p1      (nth xs (dec i))
        p2      (nth xs (dec (dec i)))
        p3-i    (rpo->index [(- r 2) p o])
        p4-i    (dec p3-i)
        p3      (nth xs p3)
        p4      (nth xs p4)]
    (+ p1 p2 p3 p4)))

(defn middle-space [xs i]
  (let [[r p o] (index->rpo i)
        p1      (nth xs (dec i))
        p2-i    (rpo->index [(- r 2) p o])
        p3-i    (dec p2-i)
        p4-i    (dec p3-i)
        p2      (nth xs p2-i)
        p3      (nth xs p3-i)
        p4      (nth xs p4-i)]
    (+ p1 p2 p3 p4)))

(defn second-to-last-space [xs i]
  (let [[r p o] (index->rpo i)
        p1      (nth xs (dec i))
        p2-i    (rpo->index [(- r 2) p (dec o)])
        p3-i    (dec p2-i)
        p2      (nth xs p2-i)
        p3      (nth xs p3-i)]
    (+ p1 p2 p3)))

(defn last-space [xs i]
  (let [[r p o] (index->rpo i)
        p1      (nth xs (dec i))
        p2-i    (rpo->index [(- r 2) p (dec (dec o))])
        p2      (nth xs p2-i)]
    (+ p1 p2)))


(defn assign-func-to-part [part]
  (condp = (count part)
    2  [second-to-last-space last-space]
    4  [start-space second-space second-to-last-space last-space]
    (HANDLE IT)))
