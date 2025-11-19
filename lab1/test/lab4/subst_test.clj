(ns lab4.subst-test
  (:require
    [clojure.test :refer :all]
    [lab4.ast :refer :all]
    [lab4.dnf :refer :all]
    [lab4.subst :refer :all]))

(deftest subst-var-basic
  (let [x (var* :x)]
    (testing "subst-var replaces matching variable with constant"
      (is (= true*
             (subst-var x :x true)))
      (is (= false*
             (subst-var x :x false))))

    (testing "subst-var does not touch other variables"
      (is (= x
             (subst-var x :y true))))))

(deftest subst-env-multiple
  (let [expr (and* (var* :x) (var* :y))]
    (testing "subst-env replaces multiple variables"
      (is (= (and* true* false*)
             (subst-env expr {:x true :y false}))))))

(deftest dnf-with-subst-on-simple
  (let [x (var* :x)
        y (var* :y)
        expr (or* x (and* y (not* x)))]
    (testing "DNF with subst on x ∨ (y ∧ ¬x) behaves as expected"
      (is (= true*
             (dnf-with-subst expr {:x true})))
      (is (= y
             (dnf-with-subst expr {:x false}))))))

(deftest dnf-with-subst-on-and-over-or
  (let [x (var* :x)
        y (var* :y)
        expr (and* x (or* y (not* x)))]
    (testing "DNF with subst on x ∧ (y ∨ ¬x)"
      (is (= y
             (dnf-with-subst expr {:x true})))
      (is (= false*
             (dnf-with-subst expr {:x false}))))))

