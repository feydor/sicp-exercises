; SICP Project 1
; see https://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/projects/
#!/usr/bin/guile -s
!#

; P1: Some simple physics
; modelling baseball motion

; position u at time t
; where a = acceleration, v = velocity
; u(t) = 0.5at^2 + vt + u
(define (position a v u t)
  (+ (* 0.5 a t t) (* v t) u))

; P2: Basic math
; when will the baseball hit the ground?
; find the y coordinate when position is 0 aka solve the position equation above

(define root1
  (lambda (a b c)
    (quadratic-eq a b c +)))

(define root2
  (lambda (a b c)
    (quadratic-eq a b c -)))

; sign is either + or -
(define (quadratic-eq a b c sign)
  (let ((discriminant (- (square b) (* 4 a c))))
    (if (positive? discriminant)
      (/ (sign (- b)
               (sqrt discriminant))
         (* 2 a))
      #f)))

; P3: Flight time
; how long will the baseball be in flight?

; 0 = 0.5at^2 + vt + u, roots are times
; using root2 in order to get positive result
; (-b - large_determinant)/21 > 0
(define (time-to-impact vertical-velocity elevation)
  (time-to-height vertical-velocity elevation 0))

(define (time-to-height vertical-velocity elevation target-elevation)
  (define gravity 9.8) ; m/s^2
  (root2 (* -0.5 gravity)
         vertical-velocity
         (- elevation target-elevation)))

; P4: Flight distance in x
; angle in degrees
(define (travel-distance-simple elevation velocity angle)
  (let ((vx (* velocity (cos (to-radian angle))))
        (vy (* velocity (sin (to-radian angle)))))
    (let ((tf (time-to-impact vy elevation)))
      (position 0 vx elevation tf))))

; P5: What's the best angle to hit?
; ~= 45 degrees
(define (find-best-angle velocity elevation)
  (define MAX_DEGREE 90)
  (define (iter deg best)
    (define (distance deg) (travel-distance-simple elevation velocity deg))
    (cond ((> deg MAX_DEGREE) best)
          ((> (distance deg) (distance best)) (iter (+ deg 1) deg))
          (else (iter (+ deg 1) best))))
  (iter 0 0))

; P6: So why aren't baseball outfields 600 ft deep?
; add drag, 0.5C rho A vel^2 
; where C = 0.5, rho = 1.25 kg/m^3 air density, A = piD^(2/4) density of ball
(define mass-baseball 0.15) ; kg
; (define beta (* 0.5 0.5 1.25 PI (pow 0.074 (/ 2 4))))



; helper procedures
(define (square n) (* n n))
(define (to-radian deg) (/ (* deg PI) 180.))
(define PI 3.14159265359)
(define (pow x n)
  (if (= n 1)
      x
      (* x (pow x (- n 1)))))

