; SICP 2.1 - Introduction to Data Abstraction
#!/usr/bin/guile -s
!#

; Ex 2.1 - rational numbers as pairs
; reduces to lowest terms by dividing gcd
; if n is negative gcd is negative otherwise positive
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cond ((and (positive? n) (negative? d))
           (cons (/ (- n) g) (/ (- d) g)))
          ((and (negative? n) (positive? d))
           (cons (/ n (- g)) (/ d (- g))))
          (else (cons (/ n g) (/ d g))))))

(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))

; rational numbers as numerators and denominators
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

; Ex 2.2 - line segments as two pairs of points
(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define s1 (make-segment (make-point 0 0) (make-point 10 10)))
(define s2 (make-segment (make-point 0 0) (make-point 10 20)))

(define (midpoint-segment seg)
  (average (end-segment seg) (start-segment seg)))

(define (average p1 p2)
  (make-point (/ (abs (- (x-point p2) (x-point p1))) 2)
              (/ (abs (- (y-point p2) (y-point p1))) 2)))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

; Ex 2.3 - rectangles in a plane
(define (area-rect r)
  (* (width-rect r) (height-rect r)))
(define (perimeter-rect r)
  (* 2 (+ (width-rect r) (height-rect r))))

(define (make-rect origin height width angle)
  (cons (cons height width) (cons origin angle)))

; from inner -> outer, width is cdr then car
(define (height-rect r) (car (car r)))
(define (width-rect r) (cdr (car r)))
(define (origin-rect r) (car (cdr r)))
(define (angle-rect r) (cdr (cdr r)))

(define r1 (make-rect (make-point 0 0) 5 10 15))

; Ex 2.4 - pairs as procedures
; (car (cons x y)) -> x and (cdr (cons x y)) -> y

; Ex 2.6 - church numerals
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f (f x)))))
(define two (lambda (f) (lambda (x) (f (f (f x))))))

; add two church numerals
(define (add  m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))

; ((zero add1) 0)), (((add-1 zero) add1) 0)), (((add-1 (add-1 zero)) add1) 0))

; Ex 2.7 - arithmetic on intervals
(define (make-interval a b) (cons a b))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

; Ex 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

; Ex 2.10
(define (div-interval x y)
  (if (or (= x 0) (= y 0))
      (error "Division by zero: ", x, y)
      (mul-interval x 
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))





; helper procedures
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
