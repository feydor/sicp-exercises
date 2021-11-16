; SICP 3.1 - Assignment and local state
#lang racket

; Ex 3.1
(define (make-accumulator initial-value)
  (let ((sum initial-value))
    (lambda (n)
      (set! sum (+ sum n))
      sum)))

; Ex 3.2
(define (make-monitored func)
  (define times-called 0)
  (define (dispatch m)
    (cond ((eq? m 'how-many-calls?) times-called)
          ((eq? m 'reset-count) (set! times-called 0))
          (else (set! times-called (+ times-called 1))
                (func m))))
  dispatch)

; Ex 3.3/3.4
(define (make-account balance password)
  (define failed-attempts 0)
  (define max-failed-attempts 7)
  (define call-the-cops (lambda (x) "The cops are on their way!"))
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (cond ((not (eq? password p))
           (set! failed-attempts (+ failed-attempts 1))
           (if (>= failed-attempts max-failed-attempts)
               call-the-cops
               (lambda (x)
                 (error "Incorrect password, Remaining attempts:"
                 (- max-failed-attempts failed-attempts)))))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

; Ex 3.5 Monte Carlo integration
(define (estimate-integral pred? x1 x2 y1 y2 trials)
  (let ((area-of-rect (* (- x2 x1) (- y2 y1))))
    (* (monte-carlo trials pred?)
       area-of-rect)))

(define (is-on-circle? r x1 y1)
  (lambda (x y)
    (<= (+ (square (- x x1)) (square (- y y1)))
        (square r))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))
  
(define (random-in-range low high)
   (let ((range (- high low)))
     (+ low (random range))))

(define (square x) (* x x))

