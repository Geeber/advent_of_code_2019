(ns advent-of-code-2019.day01)


(defn calc-total-fuel-part1
  [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (reduce + (map #(- (quot (Integer/parseInt %) 3) 2) (line-seq rdr)))))

(defn calc-total-fuel-part2
  [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (reduce
      +
      (map
        (comp (fn
                [x]
                (->> (iterate #(- (quot % 3) 2) x)
                     (drop 1)
                     (take-while pos?)
                     (reduce +)))
              #(Integer/parseInt %))
        (line-seq rdr)))))