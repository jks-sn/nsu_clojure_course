(ns lab4.ast-test
  (:require
    [clojure.test :refer :all]
    [lab4.ast :refer :all]))

(deftest constants-test
  (testing "true*/false* and true?/false?"
    (is (true? true*))
    (is (false? false*))
    (is (not (true? false*)))
    (is (not (false? true*))))

  (testing "const* normalizes booleans and AST constants"
    (is (= true*  (const* true)))
    (is (= false* (const* false)))
    (is (= true*  (const* true*)))
    (is (= false* (const* false*)))))

(deftest vars-test
  (testing "var* creates correct AST node"
    (let [x (var* :x)]
      (is (= [:var :x] x))
      (is (var? x))
      (is (= :x (var-name x)))))

  (testing "var? discriminates non-vars"
    (is (not (var? true*)))
    (is (not (var? (not* (var* :x)))))))

(deftest core-ops-test
  (testing "not* / not?"
    (let [e (not* (var* :x))]
      (is (= [:not [:var :x]] e))
      (is (not? e))
      (is (not (not? (var* :x))))))

  (testing "and* / and?"
    (let [e (and* (var* :x) (var* :y))]
      (is (= [:and [:var :x] [:var :y]] e))
      (is (and? e))
      (is (not (and? (var* :x))))))

  (testing "or* / or?"
    (let [e (or* (var* :x) (var* :y))]
      (is (= [:or [:var :x] [:var :y]] e))
      (is (or? e))
      (is (not (or? (var* :x))))))

  (testing "imp* / imp?"
    (let [e (imp* (var* :x) (var* :y))]
      (is (= [:imp [:var :x] [:var :y]] e))
      (is (imp? e))
      (is (not (imp? (var* :x)))))))

(deftest literal?-test
  (testing "literal? for plain variable"
    (is (literal? (var* :x))))

  (testing "literal? for negated variable"
    (is (literal? (not* (var* :x)))))

  (testing "literal? is false for complex expressions"
    (is (not (literal? (and* (var* :x) (var* :y)))))
    (is (not (literal? (not* (and* (var* :x) (var* :y))))))))
