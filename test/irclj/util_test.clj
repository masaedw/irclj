(ns irclj.util-test
  (:use [irclj.util] :reload-all)
  (:use [clojure.test]))

(deftest split-with+-test
  (is (= '[() ()]
         (split-with+ #(not (= 3 %)) '())))
  (is (= '[(1 2 3) ()]
         (split-with+ #(not (= 3 %)) '(1 2 3))))
  (is (= '[(1 2 3) (3)]
         (split-with+ #(not (= 3 %)) '(1 2 3 3)))))

(deftest partition-with+-test
  (is (= '(())
         (partition-with+ #(not (= 3 %)) '())))
  (is (= '((1 2))
         (partition-with+ #(not (= 3 %)) '(1 2))))
  (is (= '((1 2 3))
         (partition-with+ #(not (= 3 %)) '(1 2 3))))
  (is (= '((1 2 3) (1 2 3) (1 2 3) (1 2 3))
         (partition-with+ #(not (= 3 %)) '(1 2 3 1 2 3 1 2 3 1 2 3)))))
