(ns advent-of-code-2019.day04)


(defn check-password
  [min max n]
  (let [digits (map #(Integer/parseInt (str %)) (str n))
        digit-pairs (partition 2 1 digits)]
    (and
      (<= min n max)
      (some (partial apply =) digit-pairs)
      (every? (partial apply <=) digit-pairs))))

(defn count-passwords
  [min max check-fn]
  (->> (range min (inc max))
       (filter (partial check-fn min max))
       (count)))

(defn check-password-2
  [min max n]
  (let [digits (map #(Integer/parseInt (str %)) (str n))]
    (and
      (<= min n max)
      (->> (partition-by identity digits)
           (map count)
           (some (partial = 2)))
      (every? (partial apply <=) (partition 2 1 digits)))))