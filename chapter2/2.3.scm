; SICP 2.3 - Sequences as Conventional Interfaces 
#lang racket
(define nil '())
(define tree1 (list 1 2 3 4 5 (list 6 7 (list 8 9))))

; Ex 2.30
; square tree using cons, car, cdr
(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

; square tree using map
(define (square-tree-map tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-map sub-tree)
             (square sub-tree)))
       tree))

; Ex 2.31
; abstract square-tree-map into tree-map
; map f onto tree
(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))
       tree))

(define (square-tree2 tree) (tree-map square tree))

; Ex 2.32
; return the set of all subsets of s
; where s is a list of distinct numbers
; the set of all subsets is:
; 1. the set of all subsets excluding the first number and
; 2. '', with the first number reinserted into each subset
(define set1 (list 1 2 3))
(define (subsets s)
  (if (null? s)
      (list nil)
      ; this part excludes the first number
      (let ((rest (subsets (cdr s))))
        ; this part reinserts the first number into each subset s
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))


; processes as sequence operations
; concentrate on the signals (as lisp lists)

; mapping stage: (map square (list 1 2 3)) => (1 4 9)

; filtering stage: (filter odd? (list 1 2 3)) => (3)
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

; accumulating stage: (accumulate + 0 (list 1 2 3)) => 6
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; ennumeration stage:
; (enumerate-interval 2 5) => (2 3 4 5)
; (enumerate-tree (list 1 (list 2 (list 3)))) => (1 2 3)
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                    (enumerate-tree (cdr tree))))))

; create procedures as sequences of signals
; enumerate the three, filter the odd ones, square them, and accumulate
(define (sum-odd-square tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate cons
              nil
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))

; Ex 2.33
; everything is just an accumulate
(define (map-acc p sequence)
  (accumulate (lambda (x acc) (cons (p x) acc)) nil sequence))
(define (append-acc seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length-acc sequence)
  (accumulate (lambda (x acc) (+ acc 1)) 0 sequence))

; Ex 2.34
; evaluating a polynomial in x at a given x is an accumulation
; Horner's rule: a0 + x(a1 + ... + x(an*x + an-1))
; ex: to compute 1 + 3x + 5x^3 + x^5 at x = 2
; (horner-eval 2 (list 1 3 0 5 0 1)) => 79
(define (horner-eval x coefficient-seq)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff
                   (* higher-terms x)))
              0
              coefficient-seq))

; Ex 2.35
(define (count-leaves-acc t)
  (accumulate +
              0
              (map (lambda (sub-tree)
                     (if (leaf? sub-tree)
                         1
                         (count-leaves-acc sub-tree)))
                   t)))

; Ex 2.36
; accumulates a sequence of sequences, all of same size
;(define (accumulate-n op init seqs)




; helper procedures
(define (square n) (* n n))
(define (leaf? tree) (not (pair? tree)))
(define (fib n)
  (define (iter a b n)
    (if (= n 0)
        a
        (iter (+ a b) a (- n 1))))
  (iter 0 1 n))
