(ns test-1-3
  (:require [clojure.test :refer :all]
            [core-1-3 :as core]))

(deftest my-map-basic
  (is (= '(3 4 5) (core/my-map dec '(4 5 6))))
  (is (= '()       (core/my-map inc '())))
  (is (= '()       (core/my-map identity nil)))
  (is (= '("c" "cc" "ccc")
         (core/my-map #(apply str (repeat % \c)) [1 2 3]))))

(deftest my-map-equals-core-map
  (is (= (apply list (map inc [1 2 3 4]))
         (core/my-map inc [1 2 3 4])))
  (is (= (apply list (map str "abc"))
         (core/my-map str "abc"))))

(deftest my-filter-basic
  (is (= '(0 3 6 9)
         (core/my-filter #(zero? (mod % 3)) (range 10))))
  (is (= '()
         (core/my-filter odd? '())))
  (is (= '()
         (core/my-filter (constantly false) [1 2 3])))
  (is (= '(1 2 3)
         (core/my-filter (constantly true) [1 2 3]))))

(deftest my-filter-equals-core-filter
  (is (= (apply list (filter odd? [1 2 3 4 5]))
         (core/my-filter odd? [1 2 3 4 5])))
  (is (= (apply list (filter #(not= \a %) "abacad"))
         (core/my-filter #(not= \a %) "abacad"))))

(deftest order-preserved
  (is (= '(a b c d)
         (core/my-filter (constantly true) '(a b c d))))
  (is (= '(2 3 4)
         (core/my-map inc '(1 2 3)))))