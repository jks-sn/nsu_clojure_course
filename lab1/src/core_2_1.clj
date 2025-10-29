(ns core-2-1)

(defn heavy-function [x]
  (do (Thread/sleep 10) (* 2.0 x)))


(defn make-integrator
  [f h]
  (when (not (pos? h))
    (throw (ex-info "step must be positive" {:step h})))

  (let [mf (memoize f)
        trapezoid-area (fn [a b] (* 0.5 (+ (mf a) (mf b)) (- b a)))
        cached-integrals (atom [0.0])
        ;; ---------------------------------------------------
        ;; F_n = F_n-1 + 0.5*(f(x_n-1)+f(x_n))·h
        ;; где x_n = n·h
        ;; ---------------------------------------------------
        extend-cache (fn [n]
                       (when (>= n (count @cached-integrals)) ;; поправить на <
                         (swap! cached-integrals
                                (fn [cache]
                                  (loop [cache cache]
                                    (if (< (count cache) (inc n))
                                      (let [k (count cache)
                                            x-prev (* (dec k) h)
                                            x-next (* k h)
                                            next (+ (last cache) (trapezoid-area x-prev x-next))]
                                        (recur (conj cache next)))
                                      cache))))))]

    ;; -------------------------------------------------------
    ;; F(x) ≈ F(xₙ) + 0.5*(f(x_n)+f(x))(x−x_n), где x_n = ⌊x/h⌋·h
    ;; -------------------------------------------------------
    (fn [x]
      (when (neg? x)
        (throw (ex-info "x must be non-negative" {:x x})))

      (let [n (long (Math/floor (/ x h)))
            _ (extend-cache n)
            base (nth @cached-integrals n)
            xn (* n h)
            tail (if (== x xn) 0.0 (trapezoid-area xn x))]
        (+ base tail)))))


;; Демонстрация
(defn -main []
  (println "Memoized trapezoidal integrator demo:")
  (let [F (make-integrator heavy-function 0.5)
        xs [100.0 99.0 101.0 100.0]]
    (doseq [x xs]
      (let [start (System/nanoTime)
            result (F x)
            elapsed-ms (/ (- (System/nanoTime) start) 1e6)]
        (println (format "F(%6.1f) = %.3f   [time: %.2f ms]" x result elapsed-ms))))))



;; peek, atom, что мемоизируем, как это хранится, зачем cached-integrals