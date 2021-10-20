; SICP 1.1 - The Elements of Programming
#!/usr/bin/guile -s
!#
(define (sqrt x)
	(define (sqrt-iter guess)
		(if (good-enough? guess (improve guess))
			guess
			(sqrt-iter (improve guess))))

	; when the difference between guesses is small enough
	(define (good-enough? guess next-guess)
		(< (abs (- guess next-guess)) 0.001))

	; Newton's method of successive approximations
	(define (improve guess)
		(average guess (/ x guess)))

	(sqrt-iter 1.0))

(define (cube-root x)
	; numerical approximation
	(define (cube-root-iter guess)
		(if (good-enough? guess (improve guess))
			guess
			(cube-root-iter (improve guess))))
	
	; when the difference between gueses is small enough
	(define (good-enough? guess next-guess)
		(< (abs (- guess next-guess)) 0.001))
	
	; Newton's equation for an improved guess to the cubed root of x
	(define (improve guess)
		(/ (+ (/ x (square guess)) (* guess 2)) 3))

	(cube-root-iter 1.0))

(define (average x y)
	(/ (+ x y) 2))

(define (abs x)
	(if (< x 0)
		(- x)
		x))

(define (square x) (* x x))
