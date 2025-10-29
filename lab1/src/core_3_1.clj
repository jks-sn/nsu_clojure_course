(ns core-3-1)

(defn heavy-even? [x]
  (Thread/sleep 10)                                         ;; имитация I/O/латентности
  (even? x))

(defn split-seq
  [coll chunk-sizes]
  (lazy-seq
    (when (and (seq coll) (seq chunk-sizes))
      (let [n (first chunk-sizes)
            [h t] (split-at n coll)]
        (cons h (split-seq t (rest chunk-sizes)))))))

(defn partition-by-count
  [chunk-count collection]
  (when (<= chunk-count 0)
    (throw (ex-info "chunk-count must be positive" {:chunk-count chunk-count})))
  (let [collection-size (count collection)
        chunk-size (quot collection-size chunk-count)
        remainder (rem collection-size chunk-count)
        sizes (concat (repeat remainder (inc chunk-size))
                      (repeat (- chunk-count remainder) chunk-size))]
    (take chunk-count (split-seq collection sizes))))

(defn parallel-filter
  [pred n coll]
  (->> (partition-by-count n coll)
       (mapv #(future (doall (filter pred %))))
       (mapcat deref)
       (doall)))

(defn -main
  [& _]
  (try
    (let [coll (vec (range 500))
          n 3]
      (println "Standart filter:")
      (time (println "count =" (count (doall (filter heavy-even? coll)))))
      (println "Parallel filter:")
      (time (println "count =" (count (parallel-filter heavy-even? n coll)))))
    (finally
      (shutdown-agents))))
