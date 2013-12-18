(ns prismatic-id3.util-test
  (:require [clojure.test :refer :all]
            [prismatic-id3.util :refer :all]))

(deftest most-common-test
         (testing "if a number that occurs most is the most-common"
                  (is (= 2 (most-common + [1 2 3 4 2]))))
         (testing "if a letter that occurs most is the most-common"
                  (is (= \e (most-common id "Connect to create a newsfeed based on your interests.")))))

(deftest without-test
         (testing "if a range from 1 to 10 without 1 is the same as a range from 2 to 10"
                  (is (= (range 2 10) (without 1 (range 1 10)))))
         (testing "if a collection with multiple 1's without 1's has no 1's"
                  (is (not-any? #(= 1 %) (without 1 [1 2 3 1 4 1 5 6]))))
         (testing "if a collection has no \\a's in it, without \\a it is the same"
                  (is (let [coll [\b \e \e \f]]
                        (= (without \a coll)
                           coll))))
         (testing "null set without something is still null set"
                  (is (= (list) (without 1 (list))))))

(deftest same?-test
         (testing "if everything is the same, then success"
                  (is (same? id [1 1 1 1])))
         (testing "if some things are not the same, then fail"
                  (is (not (same? id [1 2 1 1])))))
