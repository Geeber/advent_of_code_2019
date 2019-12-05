(ns advent-of-code-2019.day02)


(def program [1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,
              1,19,1,19,9,23,1,23,13,27,1,10,27,31,2,
              31,13,35,1,10,35,39,2,9,39,43,2,43,9,
              47,1,6,47,51,1,10,51,55,2,55,13,59,1,
              59,10,63,2,63,13,67,2,67,9,71,1,6,71,
              75,2,75,9,79,1,79,5,83,2,83,13,87,1,9,
              87,91,1,13,91,95,1,2,95,99,1,99,6,0,99,
              2,14,0,0])

(defn intcode
  ([xs]
   (intcode xs 0))
  ([xs ip]
   (let [[op p1 p2 p3] (drop ip xs)]
     (cond
       (#{1 2} op) (recur (assoc xs p3 (({1 + 2 *} op) (get xs p1) (get xs p2))) (+ ip 4))
       (= 99 op) xs
       :else (throw (IllegalStateException.))))))

(defn run-program
  [noun verb]
  (first (intcode (assoc program 1 noun 2 verb))))

(defn find-inputs
  [target-result]
  (for [n (range 0 100)
        v (range 0 100)
        :let [result (run-program n v)]
        :when (= target-result result)] [n v result]))
