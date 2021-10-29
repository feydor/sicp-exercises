; SICP 2.2 - Heirarchical data and the closure property
#!/usr/bin/guile -s
!#
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


(define (scale-list items factor)
  (if (null? items)
      '()
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

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
(define (square-list items)
  (map square items))

; Ex 2.22
(define (for-each proc items)
  (define (iter items evaluate)
    (if (null? items)
        #t
        (iter (cdr items) (proc (car items)))))
  (iter items #t))


; helper procedures
(define (square n) (* n n))
