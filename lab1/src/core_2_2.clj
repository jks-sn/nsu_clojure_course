(ns core-2-2)

(defn heavy-function [x]
  (do (Thread/sleep 10) (* 2.0 x)))

(defn make-integrator
  [mf h]
  (when (not (pos? h))
    (throw (ex-info "step must be positive" {:step h})))
  (let [
        xs (map #(* h %) (range))
        fs (map mf xs)
        ;; A_k = 0.5·h·(f_k + f_{k+1})
        areas (map (fn [a b] (* 0.5 h (+ a b))) fs (rest fs))
        ;;  F_0=0, F_n = F_{n-1}+A_{n-1}
        F_nodes (cons 0.0 (reductions + areas))
        trapezoid-seg (fn [a b] (* 0.5 (+ (mf a) (mf b)) (- b a)))]
    (fn [x]
      (when (neg? x)
        (throw (ex-info "x must be non-negative" {:x x})))

      (let [n (long (Math/floor (/ x h)))
            xn (* n h)
            base (nth F_nodes n)
            tail (if (== x xn) 0.0 (trapezoid-seg xn x))]
        (+ base tail)))))

;; Демонстрация
(defn -main []
  (println "partitial-sum integrator demo:")
  (let [F (make-integrator heavy-function 0.5)
        xs [100.0 99.0 101.0 100.0]]
    (doseq [x xs]
      (let [start (System/nanoTime)
            result (F x)
            elapsed-ms (/ (- (System/nanoTime) start) 1e6)]
        (println (format "F(%6.1f) = %.3f   [time: %.2f ms]" x result elapsed-ms))))))


;; Reduce и reduction