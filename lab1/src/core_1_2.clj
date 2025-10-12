;;Задан набор символов и число n. Опишите функцию, которая возвращает список всех строк длины n,
;;состоящих из этих символов и не содержащих двух одинаковых символов, идущих подряд.
;;1.2. Перепишите программу 1.1. так, чтобы все рекурсивные вызовы были хвостовыми

(ns core-1-2)

(defn last-char [s]
  (.charAt s (dec (count s))))

(defn symbol_to_str
  ([symbols] (symbol_to_str symbols []))
  ([symbols acc]
   (if (empty? symbols)
     acc
     (recur (rest symbols) (conj acc (str (first symbols)))))))

(defn extend-one
  ([s symbols] (extend-one s symbols []))
  ([s symbols acc]
   (if (empty? symbols)
     acc
     (let [target_symbol (first symbols)]
       (recur s (rest symbols)
              (if (= (last-char s) target_symbol)
                acc
                (conj acc (str s target_symbol))))))))

(defn step-tail
  ([symbols strings] (step-tail symbols strings []))
  ([symbols strings acc]
   (if (empty? strings)
     acc
     (let [target_string (first strings)
           acc (extend-one target_string symbols acc)]
       (recur symbols (rest strings) acc)))))

(defn all-strings-tail
  [symbols len]
  (cond
    (neg? len) (throw (ex-info "n must be non-negative" {:len len}))
    (= len 0)  [""]
    (= len 1)  (symbol_to_str symbols)
    :else
    (letfn [(build [k strings]
              (if (= k len)
                strings
                (recur (inc k) (step-tail symbols strings))))]
      (build 1 (symbol_to_str symbols)))))


(defn -main []
  (doseq [s (all-strings-tail [\a \b \c] 3)]
    (println s)))


;for, loop заменить на хвостовую рекурсию