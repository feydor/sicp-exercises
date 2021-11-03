#lang racket
(define nil '())

; Persistent bugger on codewars
; persistence(39) === 3 // because 3*9 = 27, 2*7 = 14, 1*4=4
;                       // and 4 has only one digit
(define (persistence n)
  (define (iter n result)
    (if (< n 10)
        result
        (iter (digit-mult n) (+ 1 result))))
  (iter n 0))

(define (digit-mult n)
  (if (< n 10)
      n
      (* (modulo n 10)
         (digit-mult (floor (/ n 10))))))

(define x (list 1 2 44 55 66 77 88 100 111))

; TODO: Maybe do binary search by repeated improve-guess?

; binary search
; input:
; n - an integer search target
; seq - a sorted list of integers
(define (b-search target seq)
  (define (iter min max)
    (let ((i (floor (average max min))))
      (let ((val (list-ref seq i)))
        (if (= val target)
            i
            (if (< val target)
                (iter (+ 1 i) max)
                (iter min (- 1 i)))))))
  (iter 0 (- (length seq) 1)))

; returns the target in seq
; 1-indexed
(define (chop target seq)
  (cond ;((and (= (length seq) 1) (not (= target (car seq)))) (- 1))
        ((= (length seq) 1) (car seq))
        (else
         (let ((mid-point (ceiling (/ (length seq) 2))))
           (if (> target (list-ref seq (- mid-point 1)))
               (chop target (upper-half mid-point seq))
               (chop target (lower-half mid-point seq)))))))

; returns [lower, rest-of-seq]
(define (upper-half lower seq)
  (if (= lower (length seq))
      nil
      (cons (list-ref seq lower)
            (upper-half (+ lower 1) seq))))

; returns [first-half-of-seq, higher]
; so (lower-half 1 (list 6 7 8)) => (6)
(define (lower-half higher seq)
  (define (iter i)
    (if (= i higher)
        nil
        (cons (list-ref seq i)
              (iter (+ i 1)))))
  (iter 0))

(define (mid-point seq) (ceiling (/ (length seq) 2)))

(define (enumerate low high)
  (if (> low high)
      nil
      (cons low (enumerate (+ low 1) high))))

; helper procedures
(define (average x y) (/ (+ x y) 2))
; length of the list
(define (length seq)
   (if (null? seq)
      0
      (+ 1 (length (cdr seq)))))
