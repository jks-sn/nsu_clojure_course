;;Задан набор символов и число n. Опишите функцию, которая возвращает список всех строк длины n,
;;состоящих из этих символов и не содержащих двух одинаковых символов, идущих подряд.
;;1.3. Определить функции my-map и my-filter, аналогичные map (для одного списка) и filter, выразив
;их через reduce и базовые операции над списками (cons, first, concat и т.п.)

(ns core-1-3)

(defn my-map
  [function collection]
  (reduce (fn [v y] (conj v (function y))) [] collection))

(defn my-filter
  [condition collection]
  (reduce (fn [v y] (if (condition y) (conj v y) v)) [] collection))

(defn -main
  [& _args]
  (println "my-map dec '(4 5 6) =>" (my-map dec '(4 5 6))))
