(ns irclj.util
  (:import (com.ibm.icu.text CharsetDetector))
  )


(defn byte-seq->str
  "バイトシーケンスを文字列に変換する"
  ([seq]
     (byte-seq->str seq nil))
  ([seq coding]
     (let [detector (CharsetDetector.)]
       (.getString detector (into-array Byte/TYPE seq) coding))))

(defn take-to-first
  "Returns a lazy sequence of successive items from coll up to
   and including the point at which it (pred item) returns true.
   pred must be free of side-effects."
  [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (cond (empty? coll)
           nil
           (pred (first s))
           (list (first s))
           true
           (cons (first s) (take-to-first pred (rest s)))))))

(defn partition-when
  "Applies f to each value in coll, splitting it each time f returns
   true. Returns a lazy seq of lazy seqs."
  [func coll]
  (letfn [(step [f c]
                (when-let [s (seq c)]
                  (let [run (take-to-first f s)
                        res (drop (count run) s)]
                    (cons run (partition-when f res)))))]
    (lazy-seq (step func coll))))
