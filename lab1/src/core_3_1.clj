(ns core-3-1)

(defn heavy-even?
  [x]
  (Thread/sleep 5)
  (even? x))

(defn chunk-by-size
  [block-size coll]
  (when (<= block-size 0)
    (throw (ex-info "block-size must be positive" {:block-size block-size})))
  (when (seq coll)
    (let [block (take block-size coll)
          rest-elements (drop block-size coll)]
      (cons block (chunk-by-size block-size rest-elements)))))

(defn parallel-filter-blocksize
  [pred block-size coll]
  (->> (chunk-by-size block-size coll)
       (mapv (fn [block]
               (future
                 (let [tname (.getName (Thread/currentThread))]
                   (println tname))
                 (doall (filter pred block)))))
       (mapcat deref)))

(defn -main
  [& _]
  (try
    (let [data (vec (range 1000))
          block-size 10]
      (println "Data size:" (count data) ", block size:" block-size)

      (time
        (println "seq  filter count ="
                 (count (doall (filter heavy-even? data)))))

      (time
        (println "para filter count ="
                 (count (parallel-filter-blocksize heavy-even? block-size data)))))
    (finally
      (shutdown-agents))))
