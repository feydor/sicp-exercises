; SICP 1.3 - Formulating abstractions with higher-order procedures
#!/usr/bin/guile -s
!#

; summation from a to b
; params:
; a    - the starting index
; b    - the ending index
; term - a function for the terms
; next - a function to increment the index
(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

; sum of the cubes between a and b
(define (sum-cubes a b)
  (sum cube a inc b))

; sum of the integers between a and b
(define (sum-integers a b)
  (sum identity a inc b))

; term = 1 / (x * 2x), x -> x+ 4
(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(define (pi-aprox)
  (* 8 (pi-sum 1 1000)))

; numerical aproximation of the integral of f between a and b
; (integral f) =~ [f(a+dx/2) + f((a+dx)+dx/2) + f((a+2dx)+dx/2)+...]*dx
; ex: (integral cube 0 1 0.001) = 0.249999875
(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

; 1.29 - Simpson's rule
; integral(f) =~ h/3(sum of terms)
; where h = (b-a)/n
; where term = (coefficient)f(a + kh) for kth term
; where coefficient = 4 if odd, 2 if even, 1 if first or last
; params:
; n - accuracy (~1000)
; ex: (integral-simpson cube 0 1 1000) = 0.25033333333
(define (integral-simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h))))
  (define (term k)
    (* (cond ((odd? k) 4)
          ((even? k) 2)
          ((or (= k 0) (= k n)) 1))
       (y k)))
        
  (/ (* h (sum term 0 inc n)) 3))

; 1.30 iterative summation
; result -> result + (term a)
; a      -> (next a)
; ai = 0
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
    (iter a 0))

; 1.31 iterative product from a to b
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1))

; 1.32 recursive product
(define (product-recur term a next b)
  (if (> a b)
    1
    (* (term a)
       (product-recur term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

; 1.32 Wallis's product; approximation of pi/2
; pi/2 =~ (2n/(2n -1))*(2n/2n + 1) for n = 0...N
(define (wallis N)
  (define (term x)
    (* (/ (* 2 x)
          (- (* 2 x) 1))
       (/ (* 2 x)
          (+ (* 2 x) 1))))
  (product term 1.0 inc N))

(define (pi-wallis)
  (* 2 (wallis 1000)))

; 1.32 generalized sum and product into a combiner
(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a)
              (accumulate combiner null-value term (next a) next b))))

; iterative version of above
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (acc-sum term a next b)
  (accumulate-iter + 0 term a next b))

(define (acc-prod term a next b)
  (accumulate-iter * 1 term a next b))

; 1.33 generalized accumulate with an abstract condition (filter)
(define (filtered-accumulate predicate? combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (if (predicate? a)
                  (term a)
                  null-value)
              (filtered-accumulate predicate? combiner null-value
                                   term (next a) next b))))

; 1.33a sum of the squares of the prime numbers between a and b
(define (sum-prime-squares a b)
  (filtered-accumulate prime? + 0 square a inc b))

; 1.33b the product of all the positive integers < n that are relatively prime to n
; GCD(i, n) = 1
(define (product-prime-integers n)
  (define (relative-prime? i)
    (= (gcd i n) 1))
  (filtered-accumulate relative-prime? * 1 identity 1 inc n))

; f(x,y) = x(a)^2 + y(b) + (b)(a)
; where a = 1+xy, b = 1-y are local variables defined using let
(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* b a))))

; half-interval method for finding roots of an equation f(x) = 0
(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    ; test for negative and positive inputs to search
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
            (error "Values are not of opposite sign" a b)))))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
     (if (close-enough? neg-point pos-point)
       midpoint
       (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (pi-hf) (half-interval-method sin 2.0 4.0))

; finding fixed points of a function f
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; sqrt of x
; y^2 = x, 2y = (x/y + y), y = (1/2)(y + x/y)
(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

; 1.35 golden ratio as fixed-point of x -> 1 + 1/x
(define (phi)
  (fixed-point (lambda (x) (+ (/ 1 x) 1)) 1.0))

; helper procedures
(define (identity n) n)
(define (square n) (* n n))
(define (cube n) (* n n n))
(define (inc n) (+ n 1))
(define (divides? a b) (= (remainder b a) 0))
(define (positive? n) (> n 0))
(define (negative? n) (< n 0))
(define (average x y) (/ (+ x y) 2.0))

; prime? - testing for divisiblity via successive integers starting with 2
(define (prime? n)
  (= n (smallest-divisor n)))
(define (smallest-divisor n)
  (find-divisor-improved n 2))
(define (find-divisor-improved n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor-improved n (next test-divisor)))))

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

; iterative euclid's algorithm for GCD
; steps: O(logn)
(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))
