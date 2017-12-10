(ns advent-of-code-2017.day1.2-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2017.day1.2 :refer :all]))

(deftest sample-test-1
  (testing "1212 has captcha of 6"
    (is (= (solve-captcha "1212")
           6))))

(deftest sample-test-2
  (testing "1221 has captcha of 0"
    (is (= (solve-captcha "1221")
           0))))

(deftest sample-test-3
  (testing "123425 has captcha of 4"
    (is (= (solve-captcha "123425")
           4))))

(deftest sample-test-4
  (testing "123123 has captcha of 12"
    (is (= (solve-captcha "123123")
           12))))

(deftest sample-test-4
  (testing "12131415 has captcha of 4"
    (is (= (solve-captcha "12131415")
           4))))
