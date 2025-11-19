(ns lab4.dnf-test
  (:require
    [clojure.test :refer :all]
    [lab4.ast :refer :all]
    [lab4.dnf :refer [dnf]]))

(deftest dnf-literals-and-constants
  (testing "DNF of constants"
    (is (= true* (dnf true*)))
    (is (= false* (dnf false*))))

  (testing "DNF of variables and negated variables"
    (let [x (var* :x)
          nx (not* x)]
      (is (= x (dnf x)))
      (is (= nx (dnf nx))))))

(deftest dnf-simple-ops
  (let [x (var* :x)
        y (var* :y)]
    (testing "DNF of x ∨ y is x ∨ y"
      (is (= (or* x y)
             (dnf (or* x y)))))

    (testing "DNF of x ∧ y is x ∧ y (single conjunction term)"
      (is (= (and* x y)
             (dnf (and* x y)))))))

(deftest dnf-implication
  (let [x (var* :x)
        y (var* :y)]
    (testing "DNF of x -> y is ¬x ∨ y"
      (is (= (or* (not* x) y)
             (dnf (imp* x y)))))))

(deftest dnf-complex-tautology
  (let [x (var* :x)
        y (var* :y)
        z (var* :z)
        expr (or* (not* (or* (not* (imp* x y)) z))
               x
               (and* z x)
               (not* x))]
    (testing "Complex expression is tautology, DNF = true"
      (is (= true* (dnf expr))))))

(deftest dnf-distribution-example
  (let [x (var* :x)
        y (var* :y)
        expr (and* x (or* y (not* x)))
        dnf-expr (dnf expr)]
    (testing "DNF of x ∧ (y ∨ ¬x) is conjunction of literals"
      (is (= (and* x y) dnf-expr)))))

(deftest dnf-distribution-and-over-or
  (let [a (var* :a)
        b (var* :b)
        z (var* :z)
        expr      (and* a (or* b z))
        expected  (or*
                    (and* a b)
                    (and* a z))
        dnf-expr  (dnf expr)]
    (testing "DNF of a ∧ (b ∨ z) is (a ∧ b) ∨ (a ∧ z)"
      (is (= expected dnf-expr)))))