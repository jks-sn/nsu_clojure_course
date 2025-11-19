(ns test-3-1
  (:require [clojure.test :refer :all]
            [core-3-1 :as core])
  (:import (clojure.lang ExceptionInfo)))


(deftest test-chunk-by-size
  (is (= [[1 2 3] [4 5 6] [7]]
         (core/chunk-by-size 3 [1 2 3 4 5 6 7])))
  (is (thrown? ExceptionInfo
               (core/chunk-by-size 0 [1 2 3])))
  (is (= nil (core/chunk-by-size 3 []))))

(deftest test-parallel-filter-blocksize-basic
  (is (= [2 4 6 8]
         (core/parallel-filter-blocksize even? 3 (range 1 10))))
  (is (= []
         (core/parallel-filter-blocksize pos? 2 [])))
  (is (= [1 3 5 7 9]
         (core/parallel-filter-blocksize odd? 4 (range 1 10)))))

(deftest test-parallel-filter-blocksize-order
  (is (= (filter even? (range 20))
         (core/parallel-filter-blocksize even? 5 (range 20)))))
