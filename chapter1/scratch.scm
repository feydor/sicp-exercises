#!/usr/bin/guile -s
!#
(define (abs x)
  (if (< x )
      (- x)
      x))

(define (>= x y)
  (or (> x y) (= x y)))

(define (<= x y)
  (not (> x y)))

(define (fact x)
  (if (= x 1)
      1
      (* x (fact (- x 1)))))

; iterative fibonacci; fib(n)
; a -> a+b, b -> a for count < n
(define (fib n)
  (define (iter a b count)
    (if (= count n)
      a
      (iter (+ a b) a (+ count 1))))
  (iter 0 1 0))

