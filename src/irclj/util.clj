(ns irclj.util
  (:import (com.ibm.icu.text CharsetDetector))
  )

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
  (lazy-seq
   (let [[head tail] (split-with+ pred coll)]
     (cons head
           (if (empty? tail)
             (empty head)
             (partition-with+ pred tail))))))

(defn partition-when
  "Applies f to each value in coll, splitting it each time f returns
   true. Returns a lazy seq of lazy seqs."
  [f coll]
  (when-let [s (seq coll)]
    (let [run (take-to-first #(f %) s)
          res (drop (count run) s)]
      (lazy-seq
       (cons run (partition-when f res))))))

(defn byte-seq->str
  "バイトシーケンスを文字列に変換する"
  ([seq]
     (byte-seq->str seq nil))
  ([seq coding]
     (let [detector (CharsetDetector.)]
       (.getString detector (into-array Byte/TYPE seq) coding))))
