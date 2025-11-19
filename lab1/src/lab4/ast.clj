(ns lab4.ast
  "AST and basic constructors/predicates for Boolean expressions."
  (:refer-clojure :exclude [true? false? var?]))

;; === Constants ===
(def true* [:true])
(def false* [:false])



(defn true? [e] (= e true*))
(defn false? [e] (= e false*))


(defn const*
  "Return canonical constant node for true/false flags or existing nodes."
  [x]
  (cond
    (= x true*)  true*
    (= x false*) false*
    (= x true)   true*
    (= x false)  false*
    :else (throw (ex-info "Not a boolean constant" {:value x}))))

;; Variables
(defn var* [k]
  {:pre [(keyword? k)]}
  [:var k])

;; Is expression is variable
(defn var? [e] (= (first e) :var))

;; Get var name
(defn var-name [[_ k]] k)


;; Core operations: not/and/or
(defn not* [e] [:not e])
(defn not? [e] (= (first e) :not))


(defn and* [& xs] (vec (into [:and] xs)))
(defn and? [e] (= (first e) :and))


(defn or* [& xs] (vec (into [:or] xs)))
(defn or? [e] (= (first e) :or))

(defn imp* [p q] [:imp p q])
(defn imp? [e] (= (first e) :imp))


;; Utility
(defn op-args [e] (subvec e 1))


(defn literal?
  "A literal is a variable or its negation."
  [e]
  (or (var? e)
      (and (not? e) (var? (second e)))))