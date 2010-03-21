(ns irclj.util)

(defn split-with+
  "predが真の要素に加えて、最初に偽になった要素がとりだされる版のsplit-with"
  [pred coll]
  (let [[head tail] (split-with pred coll)]
    (if (empty? tail)
      [head tail]
      [(concat head (list (first tail))) (if (nil? (next tail)) () (next tail))])))

(defn partition-with+
  "split-with+を残りの要素にも再帰的に適用したシーケンスを返す"
  [pred coll]
  (let [[head tail] (split-with+ pred coll)]
    (if (empty? tail)
      (list head)
      (cons head (partition-with+ pred tail)))))
