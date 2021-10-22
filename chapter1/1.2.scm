; SICP 1.2
#!/usr/bin/guile -s
!#
(define (f n)
	(cond ((< n 3) n)
				((>= n 3) (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))))

; returns the col-th element of the row r of pascal's triangle
(define (pascal row col)
	(cond ((< row col) #f)
				((or (= col 0) (= row col)) 1)
				(else (+ (pascal (- row 1) col) (pascal (- row 1) (- col 1))))))

(define (inc n) (+ n 1))

