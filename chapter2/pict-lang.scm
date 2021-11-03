; SICP 2.2.4 - A Picture Language
#lang racket
(define nil '())

; START graphics library
(require graphics/graphics)
(open-graphics)
(define vp (open-viewport "A Picture Language" 500 500))

; drawing functions
(define draw (draw-viewport vp))
(define (clear) ((clear-viewport vp)))
(define line (draw-line vp))

;need a wrapper function so that the graphics library works with my code...
(define (vector-to-posn v)
  (make-posn (car v) (cdr v)))

(define (segments->painter segment-list)   
  (lambda (frame)     
   (for-each     
     (lambda (segment)        
      (line         
        (vector-to-posn ((frame-coord-map frame) (start-segment segment)))         
        (vector-to-posn ((frame-coord-map frame) (end-segment segment)))))      
      segment-list)))
; END graphics library

; Frame, vector, painter
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame f) (car f))
(define (edge1-frame f) (cadr f))
(define (edge2-frame f) (cadr (cdr f)))

; map the unit square into the frame
; Origin(Frame) + x * Edge1(Frame) + y * Edge2(Frame)
; so (0.5, 0.5) => ((edge1)/2, (edge2)/2)
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))
; Ex 2.46
; vector v => (x, y)
(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

; line segments => two vectors
(define (make-segment start-vec end-vec) (cons start-vec end-vec))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

; example vectors, frame, segments, painters
(define v1 (make-vect 0 20))
(define v2 (make-vect 20 0))
(define origin (make-vect 0 0))
(define frame (make-frame origin v1 v2)) ; 66x66 rect
(define left-side (make-segment origin v1))
(define top-side (make-segment v1 (add-vect v1 v2)))
(define (right-side f) (make-segment (add-vect v1 v2) v2))
(define (bottom-side f) (make-segment v2 origin))
(define tl-to-br (make-segment v1 v2))
(define tr-to-bl (make-segment (add-vect v1 v2) origin))

; draws the outlines of the frame
(define outline-painter
  (segments->painter (list left-side top-side right-side bottom-side)))

(define x-painter
  (segments->painter (list tl-to-br tr-to-bl)))

; returns a transformed painter
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))
