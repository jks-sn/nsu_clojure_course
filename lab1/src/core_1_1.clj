;;Задан набор символов и число n. Опишите функцию, которая возвращает список всех строк длины n,
;;состоящих из этих символов и не содержащих двух одинаковых символов, идущих подряд.
;;1.1. Решите задачу с помощью элементарных операций над последовательностями и рекурсии;;for и doseq - проблемы без одного

(ns core-1-1)

(def string-list-rec
  (fn [symbols len]
    (cond
      (neg? len)
      (throw (ex-info "n must be non-negative" {:len len}))

      (= len 0)
      [""]

      (= len 1)
      (map str symbols)

      :else
      (for [s (string-list-rec symbols (dec len))
            c symbols
            :when (or (empty? s) (not= (last s) c))]
        (str s c)))))

(defn -main []
  (doseq [s (string-list-rec [\a \b \c] 3)]
    (println s)))

;;for и doseq - проблемы без одного