(ns test-1-1
  (:require [clojure.test :refer :all]
            [core-1-1 :as core]))

(def symbols [\a \b \c])

(defn no-adjacent-dup? [^String s]
  (or (<= (count s) 1)
      (every? (fn [[a b]] (not= a b)) (partition 2 1 s))))

(deftest n-0-empty-only
  (is (= [""]
         (seq (core-1-1/string-list-rec symbols 0)))))

(deftest n-1-all-singletons
  (is (= #{"a" "b" "c"}
         (set (core-1-1/string-list-rec symbols 1)))))

(deftest example-n-2
  (is (= #{"ab" "ac" "ba" "bc" "ca" "cb"}
         (set (core-1-1/string-list-rec symbols 2)))))

(deftest property-no-adjacent
  (doseq [n (range 0 5)
          s (core-1-1/string-list-rec symbols n)]
    (is (no-adjacent-dup? s) (str "violates: " s))))

(deftest size-check
  (doseq [n (range 0 6)]
    (let [expected (if (zero? n) 1
                                 (* (count symbols) (long (Math/pow (dec (count symbols)) (dec n)))))
          actual   (count (core-1-1/string-list-rec symbols n))]
      (is (= expected actual) (str "n=" n)))))

(deftest empty-symbols
  (is (= [""]
         (seq (core-1-1/string-list-rec [] 0))))
  (is (= []
         (vec (core-1-1/string-list-rec [] 1)))))
