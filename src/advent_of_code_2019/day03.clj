(ns advent-of-code-2019.day03)

(defn str->path
  [s]
  (clojure.string/split s #","))

(defn path->points
  ([path]
   (path->points path [0 0]))
  ([path point]
   (if-let [[[d & ns] & rst] (seq path)]
     (let [dirs {\U [0 1] \R [1 0] \D [0 -1] \L [-1 0]}
           n (Integer. (apply str ns))
           point' (mapv + point (dirs d))]
       (lazy-seq
         (cons point' (path->points
                        (if (= n 1) rst (cons (str d (dec n)) rst))
                        point')))))))

(defn find-closest-intersection
  [p1 p2]
  (->>
    [p1 p2]
    (map (comp set path->points str->path))
    (apply clojure.set/intersection)
    (map (partial map #(Math/abs %)))
    (map (partial reduce +))
    (sort)
    (first)))

(defn path->point-distance-map
  [path]
  (->>
    (path->points (str->path path))
    (interleave (map inc (range)))
    (reverse)
    (apply hash-map)))

(defn find-minimal-paths-intersection
  [p1 p2]
  (let [p1-map (path->point-distance-map p1)
        p2-map (path->point-distance-map p2)]
    (->>
      p1-map
      (keep (fn
              [[p d]]
              (when-let [d2 (get p2-map p)]
                (+ d d2))))
      (sort)
      (first))))
