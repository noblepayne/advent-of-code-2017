(ns advent-of-code-2017.day2.1-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2017.day2.1 :refer :all]))

(def test-input-1 [[5 1 9 5]
                   [7 5 3]
                   [2 4 6 8]])

(deftest sample-test-1
  (testing "5195 is 8"
    (is (= (max-min-diff (first test-input-1))
           8))))

(deftest sample-test-2
  (testing "753is 8"
    (is (= (max-min-diff (second test-input-1))
           4))))

(deftest sample-test-3
  (testing "2468 is 6"
    (is (= (max-min-diff (nth test-input-1 2))
           6))))

(deftest sample-test-4
  (testing "test-input-1 is 18"
    (is (= (solve-puzzle test-input-1) 18))))
