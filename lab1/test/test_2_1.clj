(ns test-2-1
  (:require [clojure.test :refer :all]
            [core-2-1 :as core])
  (:import (clojure.lang ExceptionInfo)))

(defn assert-approx
  ([a b] (assert-approx a b 1.0e-9))
  ([a b eps]
   (is (<= (Math/abs ^double (- (double a) (double b)))
           (double eps))
       (format "expected ≈ %.12f, got %.12f (eps=%g)" (double b) (double a) eps))))

(deftest constant-function-integral
  (testing "f(x)=C and F(x)=C*x"
    (let [C 5.0
          f (fn [_] C)
          F (core/make-integrator f 0.5)]
      (doseq [x [0.0 0.5 1.0 2.5 10.0]]
        (assert-approx (F x) (* C x))))))

(deftest linear-function-integral
  (testing "f(x)=x and F(x)={x^2}/2)"
    (let [f (fn [x] x)
          F (core/make-integrator f 1.0e-3)]
      (doseq [x [0.0 0.1 1.0 2.3 7.7]]
        (assert-approx (F x) (/ (* x x) 2.0))))))

(deftest tail-segment-between-nodes
  (testing "correct tail"
    (let [f (fn [x] (+ 1.0 x))      ; F(x)=x+{x^2}/2
          h 0.5
          F (core/make-integrator f h)
          x 1.3
          exact (+ x (/ (* x x) 2.0))]
      (assert-approx (F x) exact))))

(deftest memoization-calls-are-reused
  (testing "repeact calls f don't repeat calculate f"
    (let [calls (atom 0)
          f (fn [x] (swap! calls inc) x)
          h 1.0
          F (core/make-integrator f h)]
      (reset! calls 0)
      (F 2.0)
      (is (= 3 @calls) "nodes 0,1,2 and 3 calls f")
      (F 2.0)
      (is (= 3 @calls) "repeat without new calls")
      (F 2.3)
      (is (= 4 @calls) "new point: 2.3"))))

(deftest negative-x-and-step-throws
  (testing "Errors: x<0 or step<=0"
    (let [F (core/make-integrator identity 0.5)]
      (is (thrown-with-msg? ExceptionInfo #"x must be non-negative"
                            (F -0.1))))
    (doseq [h [0.0 -0.5]]
      (is (thrown-with-msg? ExceptionInfo #"step must be positive"
                            (core/make-integrator identity h))))))

(deftest lazy-growth-does-not-precompute
  (testing "Lazy seq computes only on demand"
    (let [calls (atom 0)
          f (fn [x] (swap! calls inc) x)
          F (core/make-integrator f 1.0)]
      (is (= 0 @calls))
      (F 2.0)
      (is (= 3 @calls))
      (F 4.0)
      (is (<= 5 @calls 6)))))
