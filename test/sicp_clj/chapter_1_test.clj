(ns sicp-clj.chapter-1-test
  (:use clojure.test
        sicp-clj.chapter-1))

(deftest abs-test
  (is (= 1.0 (abs 1.0)))
  (is (= 1.0 (abs -1.0)))
  (is (= 0.0 (abs 0.0)))
  (is (= 1 (abs 1)))
  (is (= 1 (abs -1)))
  (is (= 0 (abs 0))))

(def a 3)
(def b 4)

(deftest ex-1-1-test
  (testing "values"
    (is (= 10 10))
    (is (= 12 (+ 5 3 4)))
    (is (= 6 (+ (* 2 4) (- 4 6))))
    (is (= 19 (+ a b (* a b))))
    (is (= false (= a b)))
    (is (= 4 (if (and (> b a) (< b (* a b)))
               b
               a)))
    (is (= 16 (cond
                (= a 4) 6
                (= b 4) (+ 6 7 a)
                :else 25)))
    (is (= 6 (+ 2 (if (> b a) b a))))
    (is (= 16 (* (cond
                   (> a b) a
                   (< a b) b
                   :else -1)
                 (+ a 1))))))

(deftest ex-1-2-test
  (testing "prefix form"
    (is (= -37/150 (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
                      (* 3 (- 6 2) (- 2 7)))))))

(deftest ex-1-3-test
  (testing "sum of squares of 2 largest of 3 numbers"
    (is (= 13
           (sum-of-squares-of-biggest-2 1 2 3)))
    (is (= 13
           (sum-of-squares-of-biggest-2 2 3 1)))))

(deftest ex-1-4-test
  (testing "compound expressions as operators"
    (is (= 2
           (a-plus-abs-b 1 1)))
    (is (= 2
           (a-plus-abs-b -1 -3)))))

(deftest ex-1-5-test
  (testing "clojure uses applicative order evaluation"
    (is (thrown? StackOverflowError
                 (test-evaluation-order 0 (p))))))

(deftest ex-1-6-test
  (testing "new-if function isn't very useful"
    (is (thrown? StackOverflowError
                 (sqrt-iter-with-new-if 1.0 2)))))

(defn error-magnitude
  [n sqrt-fn]
  (abs (- n (square (sqrt-fn n)))))

(deftest ex-1-7-test
  (testing "better-sqrt is better than sqrt for very small numbers"
    (let [err (partial error-magnitude 0.0001)]
      (is (< (err better-sqrt)
             (err sqrt)))))
  (testing "better-sqrt works (at all) for (some) very big numbers"
    (let [n 1.0E23
          err (partial error-magnitude n)]
      (is (thrown? StackOverflowError
                   (err sqrt)))
      (is (better-good-enough? (better-sqrt n) n))))
  (testing "better-sqrt still fails with reeeeeally big numbers"
    (let [n 1.0E50
          err (partial error-magnitude n)]
      (is (thrown? StackOverflowError
                   (err sqrt)))
      (is (thrown? StackOverflowError
                   (err better-sqrt))))))

(deftest ex-1-8-test
  (testing "cube roots"
    (= 3.0 (cube-root 27))))