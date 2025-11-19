(ns lab4.dnf
  "dnf transformation pipeline."
  (:require [lab4.ast :as a]
            [lab4.rewrite :refer :all]
            [lab4.simplify :refer :all]))


(defn dnf
  "Converts expression `expr` to a Disjunctive Normal Form (DNF).
  Guarantees semantic equivalence. Pipeline:
  1) rewrite derived ops to {not,and,or}
  2) push negations to literals (NNF)
  3) distribute AND over OR (sum of products)
  4) flatten & simplify (consts, dups, taut/contr)
  5) ensure DNF shape
  Returns a canonicalized DNF expression."
  [expr]
  (let [e1 (rewrite-complex expr)
        e2 (push-nots e1)
        e3 (to-dnf e2)
        e4 (simplify* e3)]
    (assert (dnf-shape? e4) (str "Result is not DNF: " e4))
    e4))