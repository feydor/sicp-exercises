#!/usr/bin/guile -s
!#
(define (square x) (* x x))

(define (sum-of-squares x y) (+ (square x) (square y)))

; returns the greater of two
(define (max x y)
	(if (>= x y)
			x
			y))

; returns the greater of three
(define (max-of-three x y z)
	(cond ((and (> x y) (> x z)) x)
				((> y z) y)
				(else z)))

(define (sum-of-max-squares x y z)
	(cond ((= x (max-of-three x y z)) (sum-of-squares x (max y z)))
				((= y (max-of-three x y z)) (sum-of-squares y (max x z)))
				(else (sum-of-squares z (max x y)))))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (>= x y)
  (or (> x y) (= x y)))

(define (<= x y)
  (not (> x y)))
