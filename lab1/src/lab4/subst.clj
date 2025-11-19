(ns lab4.subst
  (:require
    [lab4.ast :refer :all]
    [lab4.dnf :refer [dnf]]))

(defn subst-var
  [expr k value]
  (cond
    (or (true? expr) (false? expr))
    expr

    (var? expr)
    (if (= (var-name expr) k)
      (const* value)
      expr)

    (not? expr)
    (not* (subst-var (second expr) k value))

    (and? expr)
    (apply and* (map #(subst-var % k value) (op-args expr)))

    (or? expr)
    (apply or*  (map #(subst-var % k value) (op-args expr)))

    :else expr))

(defn subst-env
  [expr env]
  (reduce (fn [e [k v]]
            (subst-var e k v))
          expr
          env))

(defn dnf-with-subst
  [expr env]
  (-> expr
      dnf
      (subst-env env)
      dnf))
