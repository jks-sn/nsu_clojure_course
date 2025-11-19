(ns lab4.simplify
  "Negation pushing (NNF), distribution to DNF, flattening, and constant/dup/tautology rules."
  (:require
    [clojure.set :refer :all]
    [lab4.ast :refer :all]))


(defn push-nots
  "Return expr in NNF: negations appear only directly over variables/consts."
  [expr]
  (cond
    (or (true? expr) (false? expr) (var? expr))
    expr

    (not? expr)
    (let [e (second expr)]
      (cond
        (true? e)  false*
        (false? e) true*
        (var? e)   (not* e)
        (not? e)   (push-nots (second e))
        (and? e)   (or*  (push-nots (not* (first (op-args e)))) (push-nots (not* (second (op-args e)))))
        (or?  e)   (and* (push-nots (not* (first (op-args e)))) (push-nots (not* (second (op-args e)))))
        :else      (not* (push-nots e))))

    (and? expr)
    (apply and* (map push-nots (op-args expr)))

    (or? expr)
    (apply or*  (map push-nots (op-args expr)))

    :else
    expr))

(defn flatten-op
  "Flatten nested nodes with the same tag, keep others as-is."
  [tag maker args]
  (let [flat (mapcat #(if (= (first %) tag) (op-args %) [%]) args)]
    (apply maker flat)))

(defn flatten*
  "Flatten nested AND/OR everywhere."
  [expr]
  (cond
    (or (true? expr) (false? expr) (var? expr) (and (not? expr) (literal? (second expr))))
    expr

    (not? expr)
    (not* (flatten* (second expr)))

    (and? expr)
    (->> (op-args expr) (map flatten*) (flatten-op :and and*))

    (or? expr)
    (->> (op-args expr) (map flatten*) (flatten-op :or  or*))

    :else expr))

(defn has-contradiction?
  "Return true if args contain x and (not x) for some variable."
  [args]
  (let [vars  (set (keep #(when (var? %) (var-name %)) args))
        nvars (set (keep #(when (and (not? %) (var? (second %)))
                            (var-name (second %)))
                         args))]
    (not (empty? (intersection vars nvars)))))

(defn- simplify-and-args
  "Apply AND rules over already-simplified args vector."
  [args]
  (let [args (->> args (remove true?) distinct vec)]
    (cond
      (some false? args)     false*
      (has-contradiction? args) false*
      (empty? args)          true*
      (= 1 (count args))     (first args)
      :else                  (apply and* args))))

(defn- simplify-or-args
  "Apply OR rules over already-simplified args vector."
  [args]
  (let [args (->> args (remove false?) distinct vec)]
    (cond
      (some true? args)      true*
      (has-contradiction? args) true*
      (empty? args)          false*
      (= 1 (count args))     (first args)
      :else                  (apply or* args))))

(defn simplify-once
  "One pass of simplification."
  [expr]
  (cond
    (or (true? expr) (false? expr) (var? expr)) expr
    (and (not? expr) (literal? (second expr)))  expr
    (not? expr)   (not* (simplify-once (second expr)))
    (and? expr)   (simplify-and-args (map simplify-once (op-args expr)))
    (or?  expr)   (simplify-or-args  (map simplify-once (op-args expr)))
    :else expr))

(defn simplify*
  "Flatten + simplify."
  [expr]
  (-> expr flatten* simplify-once))

(defn- distribute
  "Distribute AND over OR to approach DNF. Assumes NNF."
  [a b]
  (cond
    (or? a) (apply or* (map #(simplify-once (distribute % b)) (op-args a)))
    (or? b) (apply or* (map #(simplify-once (distribute a %)) (op-args b)))

    (and? a) (simplify-and-args (concat (op-args a) [b]))
    (and? b) (simplify-and-args (concat [a] (op-args b)))

    :else    (simplify-once (and* a b))))

(defn to-dnf
  "Assumes expr is in NNF. Returns DNF (OR of ANDs of literals) or a literal/const."
  [expr]
  (cond
    (or (true? expr) (false? expr) (literal? expr))
    expr

    (and? expr)
    (reduce (fn [acc x] (simplify-once (distribute acc x)))
            true*
            (map to-dnf (op-args expr)))

    (or? expr)
    (simplify-or-args (map to-dnf (op-args expr)))

    :else expr))

(defn dnf-shape?
  [e]
  (or
    (literal? e)

    (true? e)
    (false? e)

    (and (and? e)
         (every? literal? (op-args e)))

    (and (or? e)
         (every? (fn [t]
                   (or (literal? t)
                       (true? t)
                       (false? t)
                       (and (and? t)
                            (every? literal? (op-args t)))))
                 (op-args e)))))
