(ns advent-of-code-2017.day2.2-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2017.day2.2 :refer :all]))

(def test-input-1
  [[5 9 2 8]
   [9 4 7 3]
   [3 8 6 5]])

(deftest sample-test-1
  (testing "row1 is 4"
    (is (= (sum-row (nth test-input-1 0))
           4))))

(deftest sample-test-2
  (testing "row2 is 4"
    (is (= (sum-row (nth test-input-1 1))
           3))))

(deftest sample-test-3
  (testing "row3 is 4"
    (is (= (sum-row (nth test-input-1 2))
           2))))

(deftest sample-test-4
  (testing "puzzle is 9"
    (is (= (solve-puzzle test-input-1) 9))))
