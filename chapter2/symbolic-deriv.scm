#lang racket
(define (atom? x) (and (not (pair? x)) (not (null? x))))
(define (accumulate combiner initial sequence)
  (if (null? sequence)
      initial
      (combiner (car sequence)
                (accumulate combiner initial (cdr sequence)))))

; Symbolic Differentiation
; '(op expr1 expr2)
; a list containing an operator followed by an expression followed by another expression
(define (deriv expr var)
  (cond [(constant? expr var) 0]
        [(same-var? expr var) 1]
        [(sum? expr var)
         (make-sum (deriv (a1 expr) var)
                   (deriv (a2 expr) var))]
        [(product? expr var)
         (make-sum
          (make-product (m1 expr)
                        (deriv (m2 expr) var))
          (make-product (deriv (m1 expr) var)
                        (m2 expr)))]
        [(exponentiation? expr var)
         (make-product (make-product (exponent expr)
                                     (make-exponentiation (base expr)
                                                          (make-sum (exponent expr)
                                                                    (- 1))))
                       (deriv (base expr) var))]
        [else #f]
   ))

(define (constant? expr var)
  (and (atom? expr)
       (not (eq? var expr))))

(define (same-var? expr var)
  (and (atom? expr)
       (eq? var expr)))

(define (sum? expr var)
  (and (not (atom? expr))
       (eq? (car expr) '+)))

(define (product? expr var)
  (and (not (atom? expr))
       (eq? (car expr) '*)))

(define (exponentiation? expr var)
  (and (not (atom? expr))
       (eq? (car expr) '**)))

(define (make-exponentiation base exp)
  (cond [(=number? exp 0) 1]
        [(=number? exp 1) base]
        [else (list '** base exp)]))

(define (make-sum a1 a2)
  (cond [(and (number? a1) (number? a2))
         (+ a1 a2)]
        [(=number? a1 0) a2]
        [(=number? a2 0) a1]
        [else (list '+ a1 a2)]))

(define (make-product m1 m2)
  (cond [(and (number? m1) (number? m2))
         (* m1 m2)]
        [(and (number? m1) (= m1 1))
         m2]
        [(and (number? m2) (= m2 1))
         m1]
        [(and (number? m1) (= m1 0))
         0]
        [(and (number? m2) (= m2 0))
         0]
        [else (list '* m1 m2)]))

(define (=number? x num)
  (and (number? x) (= x num)))

(define op car)
(define a1 cadr)
(define (a2 s)
  (accumulate make-sum 0 (cddr s)))
(define m1 cadr)
(define (m2 p)
  (accumulate make-product 1 (cddr p)))
(define base cadr)
(define exponent caddr)

; an example of usage
; f(x) = ax^2 + bx + c
; df(x)/dx = 2ax + b
(define quadratic
  '(+ (* a (* x x))
      (+ (* b x)
         c)))
(define deriv-quadratic (deriv quadratic 'x))

(define d-quadratic
  '(+ (+ (* a
            (+ (* x 1)
               (* 1 x)))
         (* 0
            (* x x)))
      (+ (+ (* b 1)
            (* 0 x))
         0)))

(define expn '(** x 15)) ; (deriv expn 'x) => 15x^14
