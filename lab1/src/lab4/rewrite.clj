(ns lab4.rewrite
  (:require
    [lab4.ast :refer [var* not* and* or* true* false* var? not? and? or? true? false? op-args]]))

(def complex-op-rewrite
  "Map from op keyword to a function that rewrites a node [op & args]
  to an equivalent expression using only a/not*/and*/or*."
  { :imp (fn [[_ p q]] (or* (not* p) q))
   ;:xor (fn [[_ p q]] (or* (and* p (not* q))
   ;                          (and* (not* p) q)))
   })


(defn rewrite-complex
  "Recursively rewrite any derived operations in expr to {not,and,or}."
  [expr]
  (cond
    (true? expr) true*
    (false? expr) false*
    (var? expr) expr


    (not? expr) (let [e' (rewrite-complex (second expr))]
                    (not* e'))


    (and? expr) (apply and* (map rewrite-complex (op-args expr)))
    (or? expr) (apply or* (map rewrite-complex (op-args expr)))


    :else (let [op (first expr)
                f (get complex-op-rewrite op)]
            (if f
              (rewrite-complex (f expr))
              (throw (ex-info "Unknown operator; add a rewrite rule"
                              {:op op :expr expr}))))))