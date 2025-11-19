(ns core-3-2)

(defn heavy-even?
  [x]
  (Thread/sleep 5)
  (even? x))

(defn parallel-filter-lazy
  [pred block-size coll]
  (when (<= block-size 0)
    (throw (ex-info "block-size must be positive" {:block-size block-size})))
  (let [chunks (partition-all block-size coll)]
    (mapcat identity
            (pmap #(doall (filter pred %)) chunks))))

(defn -main
  [& _]
  (try
    (let [data (range 1000)
          block-size 10]
      (println "Data size:" (count data) ", block size:" block-size)

      (time
        (println "seq  filter count ="
                 (count (doall (filter heavy-even? data)))))

      (time
        (println "para filter count ="
                 (count (doall (parallel-filter-lazy heavy-even? block-size data)))))

      (time
        (println "first 20 even numbers via parallel-filter-lazy over (range):"
                 (take 500 (parallel-filter-lazy heavy-even? 10 (range))))))
    (finally
      (shutdown-agents))))
