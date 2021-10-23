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
  (define (fr-iter a b c n)
    (if (< n 3)
        a
        (fr-iter (+ a (* 2 b) (* 3 c))
                 a
                 b
                 (- n 1))))

  (if (< n 3)
      n
      (fr-iter 2 1 0 n)))


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

; 1.17 iterative multiplication
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

(define (square n) (* n n))
(define (inc n) (+ n 1))
(define (dec n) (- n 1))
(define (halve n) (/ n 2))
(define (double n) (+ n n))
