;;1.4. Решите задачу с помощью элементарных операций над последовательностями и функционалов
;map/reduce/filter

(ns core-1-4
  (:require [core-1-3 :refer [my-map my-filter]]))

(defn- expand-one [string symbols]
  (let [last_character (last string)]
    (my-map #(str string %)
            (my-filter #(not= last_character (first %)) symbols))))

(defn- expand-step [acc symbols]
  (reduce concat
          (my-map #(expand-one % symbols) acc)))

(defn all-strings
  [symbols len]
  (cond
    (neg? len) (throw (ex-info "n must be non-negative" {:n len}))
    (zero? len) (list "")
    (empty? symbols) []
    :else
    (let [symbols (my-map str symbols)]
      (if (= len 1)
        symbols
        (nth (iterate #(expand-step % symbols) symbols) (dec len))))))

(defn -main []
  (doseq [s (all-strings [\a \b \c] 3)]
    (println s)))

;; my-map, iterate