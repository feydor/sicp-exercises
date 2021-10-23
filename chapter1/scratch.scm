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
