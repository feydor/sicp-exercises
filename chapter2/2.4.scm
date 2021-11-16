; SICP 2.4 - Multiple Representations for Abstract Data
#lang racket
(define (atom? x) (and (not (pair? x)) (not (null? x))))

(require rnrs/base-6)
(require rnrs/mutable-pairs-6)

; makes a table for storing procedures
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

; complex numbers can be represented in two forms:
; 1. rectangular (real and imaginary components)
; 2. polar (magnitude and angle)

; generic procedures - procedures that can operate with multiple representations of complex data
; using data objects that have type tags

; concrete implementations of constructors/selectors
; whether rectangular or polar
; add a tag to the data to mark which representation
; (list type-tag contents)

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)
  (eq? (type-tag z) 'polar))

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

; data-directed version of symbolic differentiation
; use of dispatch-table
(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (variable? x) (symbol? x))
(define (same-variable? expr var)
  (and (atom? expr)
       (eq? var expr)))

(define (install-sum-procedures)
  ;; internal procedures
  (define (addend expr) (cadr expr))
  (define (augend expr) (caddr expr))
  (define (=number? x y) (and (number? x) (number? y) (= x y)))
  (define (make-sum x y)
    (cond [(and (number? x) (number? y)) (+ x y)]
          [(=number? x 0) y]
          [(=number? y 0) x]
          [else (list '+ x y)]))
  (define (deriv expr var)
    (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
  ;; interface
  (put 'deriv '+ deriv)
  )
(install-sum-procedures)


