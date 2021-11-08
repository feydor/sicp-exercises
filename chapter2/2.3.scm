; SICP 2.3 - Symbolic Data
#lang racket
(define nil '())

; set as a list of elements where none may appear twice
; empty set is the empty list aka (null? set)
(define set1 (list 'x 3 77 'z))
(define set2 (list 15 177 'z 3))

; is x an element of the set?
; time: O(n) (search entire list)
(define (element-of-set? x set)
  (cond [(null? set) #f]
        [(equal? x (car set)) #t]
        [else (element-of-set? x (cdr set))]))

; return (set x)
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

; only elements appearing in both sets
; time: O(n^2) b/c using element-of-set?
(define (intersection-set set1 set2)
  (cond [(or (null? set1) (null? set2)) '()]
        [(element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2))]
        [else (intersection-set (cdr set1) set2)]))

; the set containing elements that appear in either set
(define (union-set set1 set2)
  (accumulate adjoin-set set1 set2))

; Sets as ordered lists
; only numbers
(define oset1 (list 5 6 7 9))
(define oset2 (list 7 8 9 10))

; time: O(n/2)
(define (element-of-ordered-set? x set)
  (cond [(null? set) #f]
        [(= x (car set)) #t]
        [(< x (car set)) #f]
        [else (element-of-set? x (cdr set))]))

; time O(n) (sum of set1 and set2)
(define (intersection-ordered-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond [(= x1 x2)
               (cons x1
                     (intersection-ordered-set (cdr set1) (cdr set2)))]
              [(< x1 x2)
               (intersection-ordered-set (cdr set1) set2)]
              [(< x2 x1)
               (intersection-ordered-set set1 (cdr set2))]))))

(define (adjoin-ordered-set x set)
  (if (element-of-ordered-set? x set)
      set
      (cons x set)))

(define (accumulate combiner initial sequence)
  (if (null? sequence)
      initial
      (combiner (car sequence)
                (accumulate combiner initial (cdr sequence)))))

        