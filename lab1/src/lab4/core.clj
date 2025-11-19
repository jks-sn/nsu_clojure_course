(ns lab4.core
  (:gen-class)
  (:require
    [lab4.ast :refer :all]
    [lab4.dnf :refer [dnf]]
    [lab4.subst :refer [dnf-with-subst]]))

(def expr-1
  "x ∨ y."
  (or* (var* :x) (var* :y)))

(def expr-2
  "x -> y"
  (imp* (var* :x) (var* :y)))

(def expr-3
  "¬(!(x -> y) ∨ z) ∨ x ∨ (z ∧ x) ∨ ¬x"
  (or*
    (not*
      (or*
        (not* (imp* (var* :x) (var* :y)))
        (var* :z)))
    (var* :x)
    (and* (var* :z) (var* :x))
    (not* (var* :x))))

(def expr-4
  "x ∧ (y ∨ ¬x)."
  (and*
    (var* :x)
    (or* (var* :y)
         (not* (var* :x)))))

(def expr-5
  "(x | x) | (x & y) | false"
  (or* (or* (or* (var* :x) (var* :x)) (and* (var* :x) (var* :y))) false*)
  )

;; ---------------------------------------------------------------------------
;; Утилита красивого вывода
;; ---------------------------------------------------------------------------

(defn print-example
  [title expr]
  (println "======================================")
  (println title)
  (println "Исходное выражение (AST):")
  (prn expr)
  (println "DNF:")
  (prn (dnf expr))
  (println))


;; ---------------------------------------------------------------------------
;; Точка входа
;; ---------------------------------------------------------------------------

(defn -main
  [& _args]
  ;; Пример 1: x ∨ y
  (print-example "Пример 1: x ∨ y" expr-1)

  ;; Пример 2: импликация x -> y
  (print-example "Пример 2: импликация x -> y" expr-2)

  ;; Пример 3: сложное выражение с импликацией и отрицаниями
  (print-example "Пример 3: сложное выражение" expr-3)

  ;; Пример 4: демонстрация подстановки
  (println "======================================")
  (println "Пример 4: подстановка в x ∨ (y ∧ ¬x)")
  (println "Исходное выражение:")
  (prn expr-4)
  (println "DNF исходного выражения:")
  (prn (dnf expr-4))

  (println "\nПодставляем x = true, DNF результата:")
  (prn (dnf-with-subst expr-4 {:x true}))

  (println "\nПодставляем x = false, DNF результата:")
  (prn (dnf-with-subst expr-4 {:x false}))

  (println "AAAAAAAAAAAAA")
  (prn (dnf expr-5))
  (prn (dnf-with-subst expr-5 {:y true :x false}))
  (println "\nГотово."))
