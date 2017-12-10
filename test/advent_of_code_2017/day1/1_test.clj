(ns advent-of-code-2017.day1.1-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2017.day1.1 :refer :all]))

(deftest sample-test-1
  (testing "1122 has captcha of 3"
    (is (= (solve-captcha "1122")
           3))))

(deftest sample-test-2
  (testing "1111 has captcha of 4"
    (is (= (solve-captcha "1111")
           4))))

(deftest sample-test-3
  (testing "1234 has captcha of 0"
    (is (= (solve-captcha "1234")
           0))))

(deftest sample-test-4
  (testing "91212129 has captcha of 9"
    (is (= (solve-captcha "91212129")
           9))))
