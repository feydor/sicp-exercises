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
