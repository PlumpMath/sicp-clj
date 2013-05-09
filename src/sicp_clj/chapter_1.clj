(ns sicp-clj.chapter-1)

(defn square
  [x]
  (* x x))

(defn abs
  [x]
  (cond
    (> x 0) x
    (= x 0) 0
    (< x 0) (- x)))

; Exercise 1.3. Define a procedure that takes three numbers as arguments and
; returns the sum of the  squares of the two larger numbers.
(defn sum-of-squares-of-biggest-2
  [x y z]
  (->> [x y z]
       sort
       (drop 1)
       (map #(* % %))
       (apply +)))

; Exercise 1.4. Observe that our model of evaluation allows for combinations
; whose operators are compound expressions. Use this observation to describe the
; behavior of the following procedure:
(defn a-plus-abs-b
  [a b]
  ; the first expression in this form returns a function, either + or -,
  ; which is then applied to the arguments a and b as usual.
  ((if (> b 0) + -) a b))

; Exercise 1.5. Ben Bitdiddle has invented a test to determine whether the
; interpreter he is faced with is  using applicative-order evaluation or normal-
; order evaluation. He defines the following two procedures:
(defn p [] (p))

(defn test-evaluation-order
  [x y]
  (if (= x 0)
    0
    y))


; Exercise 1.6. Alyssa P. Hacker doesn't see why if needs to be provided as a
; special form. ``Why can't I  just define it as an ordinary procedure in terms
; of cond?'' she asks. Alyssa's friend Eva Lu Ator claims  this can indeed be
; done, and she defines a new version of if:
(defn new-if
  [predicate then-clause else-clause]
  (cond
    predicate then-clause
    :else else-clause))

(defn average
  [x y]
  (/ (+ x y) 2))

(defn improve
  [guess x]
  (average guess (/ x guess)))

(defn good-enough?
  [guess x]
  (< (abs (- (square guess)
             x))
     0.001))

(defn sqrt-iter
  [guess x]
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
               x)))

(defn sqrt
  [x]
  (sqrt-iter 1.0 x))

(defn sqrt-iter-with-new-if
  [guess x]
  (new-if (good-enough? guess x)
          guess
          ; the following form will always be evaluated,
          ; so even if `guess` is good enough, we keep
          ; on trying harder for no reason and we overflow
          ; the stack
          (sqrt-iter-with-new-if (improve guess x)
                                 x)))


