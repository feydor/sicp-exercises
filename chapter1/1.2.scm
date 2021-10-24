; SICP 1.2
#!/usr/bin/guile -s
!#

; ex 1.11 - recursive fn
; f(n) = n, n < 3
; f(n) = f(n-1) + 2f(n-2) + 3f(n-3) 
(define (f n)
  (cond ((< n 3) n)
        ((>= n 3) (+ (f (- n 1))
                     (* 2 (f (- n 2)))
                     (* 3 (f (- n 3)))))))

; ex 1.11 - iterative
; a -> a + 2b + 3c
; b -> a
; c -> b
(define (fr n)
  (if (< n 3)
      n
      (fr-iter 2 1 0 n)))
(define (fr-iter a b c n)
  (if (< n 3)
      a
      (fr-iter (+ a (* 2 b) (* 3 c))
               a
               b
               (- n 1))))


; 1.12 - returns the col-th element of the row r of pascal's triangle
(define (pascal row col)
  (cond ((< row col) #f)
        ((or (= col 0) (= row col)) 1)
        (else (+ (pascal (- row 1) col)
                 (pascal (- row 1) (- col 1))))))

; 1.16 - iterative exponentiation: b^n
(define (fast-expt b n)
  (if (= n 1)
      b
      (fast-expt-iter 1 b n)))

(define (fast-expt-iter a b n)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter a
                                   (square b)
                                   (/ n 2)))
        (else (fast-expt-iter (* b a)
                              b
                              (dec n)))))

; 1.17 recursive multiplication
(define (multr a b)
  (cond ((= b 0) 0)
        ((even? b) (double (multr a (halve b))))
        (else (+ a (multr a (- b 1))))))

; 1.18 iterative multiplication
(define (mult a b)
  (if (= b 0)
      0
      (mult-iter 0 a b)))

(define (mult-iter sum a b)
  (cond ((= b 0) sum)
        ((even? b) (mult-iter
                     sum
                     (double a)
                     (halve b)))
        (else (mult-iter
                (+ sum a)
                a
                (- b 1)))))

; 1.19 fib-iter, O(logn) steps 
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                  b
                  (+ (square p) (square q))
                  (+ (* 2 q p) (square q))
                  (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

; recursive fibonacci
(define (fibr n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fibr (- n 1)) (fibr (- n 2))))))

; iterative fibonacci
; a->a+b, b->a when f(0) = 0, f(1) = 1
; f(n) = a + b
(define (fibi n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (fibi-iter 0 1 n))))
(define (fibi-iter a b n)
  (if (= n 0)
       a
       (fibi-iter (+ a b) a (- n 1))))

; halve via succesive subtraction
; n should be even, otherwise halves the closest even (n - 1)
(define (half n)
  (cond ((= n 0) 0)
        ((odd? n) (half-iter (- n 1) 0))
        (else (half-iter n 0))))
(define (half-iter n sub-count)
  (if (= n sub-count)
    n
    (half-iter (- n 1) (+ sub-count 1))))

(define (square n) (* n n))
(define (inc n) (+ n 1))
(define (dec n) (- n 1))
(define (halve n) (/ n 2))
(define (double n) (+ n n))
