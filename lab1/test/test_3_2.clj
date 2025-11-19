(ns test-3-2
  (:require [clojure.test :refer :all]
            [core-3-2 :as core]))

(deftest test-parallel-filter-lazy-basic
  (is (= [2 4 6 8]
         (take 4 (core/parallel-filter-lazy even? 2 (range 1 10)))))
  (is (= [1 3 5 7 9]
         (take 5 (core/parallel-filter-lazy odd? 3 (range 1 10))))))

(deftest test-parallel-filter-lazy-infinite
  (is (= (range 0 20 2)
         (take 10 (core/parallel-filter-lazy even? 5 (range))))))

(deftest test-parallel-filter-lazy-order
  (is (= (filter even? (range 30))
         (doall (core/parallel-filter-lazy even? 4 (range 30))))))
