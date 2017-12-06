(ns advent-of-code-2017.day3.1
  (:gen-class))

;;   teribble 3

(defn convert-coords [[x y] s]
  (let [center (int (Math/floor (/ s 2)))]
    [(- x center) (- y center)]))

(defn make-ring [s]
  (let [start (inc (* (- s 2) (- s 2)))
        end   (* s s)]
    (range start (inc end))))



(defn find-row [n]
  (let [root (int (Math/ceil (Math/sqrt n)))]
    (if (even? root)
        (inc root)
        root)))


(defn min-dist [n]
  (int (/ n 2)))

(defn max-dist [n]
  (dec n))


(defn dist-cycle [n]
  (let [mi (min-dist n)
        ma (max-dist n)
        p2 (range mi (inc ma))
        p1 (->> p2 (drop 1) reverse (drop 1))
        f  (concat p1 p2)]
    f))

(defn thing [n]
  (zipmap (make-ring n)
          (drop 1 (reverse (cycle (dist-cycle n))))
          ))


(defn distance-to-port [n]
  (let [m    (find-row n)
        ring (make-ring m)
        dist (dist-cycle m)
        dmap (zipmap
              ring
              (cycle dist))]
    (get dmap n)))



(defn )





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn anagrams? [w1 w2]
  (let [c1 (count w1)
        c2 (count w2)
        f1 (frequencies w1)
        f2 (frequencies w2)]
    (and (= c1 c2)
         (= f1 f2))))


(defn test-line [line]
  (let [pline (s/split line #" ")
        combs (combo/combinations pline 2)
        things (filter #(anagrams? (first %) (second %)) combs)]
    (= 0 (count things))))

(count (filter test-line inp))
