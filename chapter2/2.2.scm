; SICP 2.2 - Heirarchical data and the closure property
;#!/usr/bin/guile -s
;!#
#lang racket
(define nil '())

; list operations

; list index
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
(define squares (list 1 4 9 16 25))
(define odds (list 1 3 5 7))

; list length
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (append src dst)
  (if (null? src)
      dst
      (cons (car src) (append (cdr src) dst))))

; Ex 2.17
; returns the last element in the list
(define (last-pair items)
  (if (null? (cdr items)) ; last item
      items
      (last-pair (cdr items))))

; Ex 2.18
(define (reverse items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items) (cons (car items) result))))
  (iter items '()))

; Ex 2.19 - change counting with lists
; (cc 100 us-coins) => 292
; order of coin-values does not matter as all values are iterated
; and summation is commutative
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define (first-denomination coins) (car coins))
(define (except-first-denomination coins) (cdr coins))
(define (no-more? coins) (null? coins))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

; Ex 2.20 - arbitrary arguments with dotted-tail notation
; returns a list of all arguments with the same even-odd parity as the first argument
(define (same-parity-iter n . args)
  (define (iter args result)
    (cond ((null? args) (reverse result))
          ((same-parity? (car args) n)
           (iter (cdr args) (cons (car args) result)))
          (else (iter (cdr args) result))))
  (iter args (list n)))

; are both the same even-odd parity?
(define (same-parity? x y)
  (if (even? x)
      (even? y)
      (odd? y)))


;(define (scale-list items factor)
;  (if (null? items)
;      '()
;      (cons (* (car items) factor)
;            (scale-list (cdr items) factor))))

; map proc over list
(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor)) items))

; Ex 2.21
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list (cdr items)))))
;(define (square-list items)
;  (map square items))

; Ex 2.23
(define (for-each proc items)
  (define (iter items evaluate)
    (if (null? items)
        #t
        (iter (cdr items) (proc (car items)))))
  (iter items #t))

; trees as lists of lists
(define t (cons (list 1 2) (list 3 4)))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define (leaf? x) (not (pair? x)))

; Ex 2.24
; (list 1 (list 2 (list 3 4)))
; '(1 (2 (3 4)))

; Ex 2.25 - get 7 with car/cdr
(define list1 (list 1 3 (list 5 7) 9)) ; (car (cdr (car (cdr (cdr list1)))))
(define list2 (list (list 7)))         ; (car (car list2))
; (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car(cdr list3))))))))))))
(define list3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

; Ex 2.27
; if head is pair, cons deep-reverse of that pair onto result
; else cons the head (a leaf node) onto result
(define (deep-reverse items)
    (define (iter x result)
      (cond ((null? x) result)
            ((pair? (car x))
             (iter (cdr x) (cons (deep-reverse (car x)) result)))
            (else (iter (cdr x) (cons (car x) result)))))
    (iter items nil))

; Ex 2.28
; (fringe (list (list 1 2) (list 3 4))) => (1 2 3 4)
; if head is pair, append left and right branches
; else cons node onto list and loop
(define (fringe x)
  (define (tree-head? x) (pair? (car x)))
  (cond ((null? x) nil)
        ((tree-head? x) (append (fringe (car x))
                                (fringe (cdr x))))
        (else (cons (car x) (fringe (cdr x))))))

; Ex 2.29
; binary mobile, a left and right branch
; each branch has either a weight or a binary mobile
(define (make-mobile left right)
  (list left right))

; branch, a length (number) and a structure (number or mobile)
(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))
(define (branch-length b) (car b))
(define (branch-structure b) (cadr b))

(define mb1 (make-mobile (make-branch 1 5) (make-branch 2 10)))

; balanced mobile with total weight of 10 on both branches
(define mb2
  (make-mobile (make-branch 10 1)
               (make-branch 2 (make-mobile (make-branch 4 1)
                                           (make-branch 1 4)))))

(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (if (pair? structure)
        (total-weight structure)
        structure)))

; total weight of the entire mobile
(define (total-weight mobile)  
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (branch-torque branch)
   (* (branch-length branch)
      (branch-weight branch)))

(define (branch-balanced? branch)
  (let ((structure (branch-structure branch)))
  (if (pair? structure)
      (balanced? structure)
      #t)))

; if the torque applied to the left is equal to that applied to the right
; and both branches are balanced
(define (balanced? mobile)
  (and (= (branch-torque (left-branch mobile))
          (branch-torque (right-branch mobile)))
       (branch-balanced? (left-branch mobile))
       (branch-balanced? (right-branch mobile))))

; mapping over trees
(define tree1 (list 1 2 3 4 5 (list 6 7 (list 8 9))))

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))
  

; helper procedures
(define (square n) (* n n))
(define (fib n)
  (define (iter a b n)
    (if (= n 0)
        a
        (iter (+ a b) a (- n 1))))
  (iter 0 1 n))
