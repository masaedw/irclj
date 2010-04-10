(ns irclj.haskell)

(defn accum-l [f e coll]
  (if (empty? coll)
    (empty coll)
    (let [pair (f e (first coll))]
      (lazy-seq (cons pair (accum-l f (first pair) (rest coll)))))))

(defn pseudo-map-accum-l [f e coll]
  (map second (accum-l f e coll)))

(defn map-accum-l [f e coll]
  (let [pair-coll (accum-l f e coll)]
    [(first (last pair-coll))
     (map second pair-coll)]))
