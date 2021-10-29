; SICP 1.3-2 - Formulating abstractions with higher-order procedures
#!/usr/bin/guile -s
!#
(define tolerance 0.00001)

; 1.3.4 - procedures as return values
; finding fixed points of a function f
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess) (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define average-damp
  (lambda (f)
    (lambda (x) (average (f x) x))))

; aprroximate sqrt as fixed-point of y->x/y with explicit average-damp
; fixed-point of y->x/y
(define (sqrt-fp x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

; cube root as fixed-point of y-> x/y^2
(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

; Newton's method:
; Newton's method is the fixed point of the newton transform on g
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

; Newton Transform:
; f(x) = x - g(x)/ Dg(x)
; where Dg(x) is the derivative of g evaluated at x
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

; The Derivative:
; Dg(x) = (g(x + dx) - g(x))/dx
; where dx is sufficently small (~0.00001)
(define (deriv g)
  (define dx 0.00001)
  (lambda (x) (/ (- (g (+ x dx)) (g x))
                 dx)))

; Newton's method on the square root
; g(x) = y^2 - x b/c
; sqrt(x) = the y such that y>=0 and y^2 = x
(define (sqrt-nm x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0)) 

; Newton's method on any n root
; g(x) = y^n - x
; params: npow - procedure to compute y^n
; ex: quad-root of 16 = (nroot 16 (nth-power 4))
(define (nroot x npow)
  (newtons-method (lambda (y) (- (npow y) x)) 1.0))

; returns a procedure that transforms x -> x^n
(define (nth-power n)
  (lambda (x)
    (define (pow result base exp)
      (cond ((= exp 0) result)
            ((even? exp) (pow result
                              (square base)
                              (/ exp 2)))
            (else (pow (* result base)
                       base
                       (- exp 1)))))

    (if (= n 1)
        x
        (pow 1 x n))))

; general abstraction of newton's method
; a fixed point of a transformed g
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt-fp-a x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))
(define (sqrt-nm-a x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))

; Ex 1.40
; (newtons-method (cubic a b c) 1) to approximate zeros for:
; x^3 + ax^2 + bx + c
(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

(define (solve-cubic a b c)
  (newtons-method (cubic a b c) 1))

; Ex 1.41
; applies a procedure of one argument twice
; so (((double (double double)) inc) 5) returns 16+5 = 21
(define (double procedure)
  (lambda (x)
    (procedure (procedure x))))

; Ex 1.42
; composition f after g => x->f(g(x))
; so ((compose square inc) 6) = 49
(define (compose f g)
  (lambda (x)
    (f (g x))))

; Ex 1.43
; returns the nth repeated application of f
; so ((repeated square 2) 5) = 5^4 = 625
(define (repeated f n)
  (lambda (x)
    (define (repeat f count)
      (if (>= count n)
          (f x)
          (repeat (compose f f) (+ count 2))))
    (cond ((< n 1) (error "Invalid argument n:" n))
          ((= n 1) (f x))
          (else (repeat f 0)))))

; Ex 1.44
; smoothed(f) is the function whose value at point x is the average of:
; f(x-dx), f(x), f(x+dx) for a small number dx
(define (smooth f)
  (define dx 0.00001)
  (lambda (x)
    (average (f (- x dx))
             (f x)
             (f (+ x dx)))))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

; Ex 1.45
; nroot using repeated applications of average-damp and fixed-point
; y->x/(y^n-1)
(define (nroot-fp x n)
  (fixed-point
   ((repeated average-damp (min-damp n))
   (lambda (y)
     (/ x (power y (- n 1)))))
   1.0))

(define (min-damp n)
  (floor (log n)))

; Ex 1.46
; generalized numerical improvement
; params:
; good-enough? - a procedure to tell if a guess is good enough
; improve - a procedure to improve the guess
; returns:
; a procedure that takes a guess and keeps improving it until good-enough
(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  (lambda (guess)
    (iter guess)))
  
; good-enough? = abs(guess^2 - x) > tolerance
; improve      = average(guess x/guess) 
(define (sqrt-improve x)
  (define tolerance 0.001)
  (define initial-guess 1.0)
  ((iterative-improve (lambda (guess) (< (abs (- (square guess) x))
                                         tolerance))
                      (lambda (guess) (average guess
                                               (/ x guess))))
   initial-guess))

; good-enough? = abs(guess - f(guess)) > tolerance
; improve      = f(guess) 
(define (fixed-point-improve f initial-guess)
  (define tolerance 0.0001)
  ((iterative-improve (lambda (guess) (< (abs (- guess (f guess)))
                                        tolerance))
                     (lambda (guess) (f guess)))
   initial-guess))


; helper procedures
(define (identity n) n)
(define (square n) (* n n))
(define (cube n) (* n n n))
(define (inc n) (+ n 1))
(define (divides? a b) (= (remainder b a) 0))
(define (positive? n) (> n 0))
(define (negative? n) (< n 0))
(define (average x y) (/ (+ x y) 2.0))
(define (average x y z) (/ (+ x y z) 3.0))
(define (average x y) (/ (+ x y) 2.0))
(define (power x n)
  (if (= n 1)
      x
      (* x (power x (- n 1)))))
