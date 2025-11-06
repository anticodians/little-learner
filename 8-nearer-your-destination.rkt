#lang racket
; Chapter 8
(require malt)
(newline)

; Data sets
(define line-xs (vector 2.0 1.0 4.0 3.0))
(define line-ys (vector 1.8 1.2 4.2 3.3))

(define plane-xs
  (tensor
   (tensor 1.0 2.05)
   (tensor 1.0 3.0)
   (tensor 2.0 2.0)
   (tensor 2.0 3.91)
   (tensor 3.0 6.13)
   (tensor 4.0 8.09)))

(define plane-ys
  (tensor 13.99 15.99 18.0 22.4 30.2 37.94))

; Speeding up the updates, rather than allowing them
; to slow down as the error gradient reduces
(declare-hyper mu)

; mu is for 'momentum' (I think!)
(define velocity-i
  (lambda (p)
    (list p (zeroes p))))

; What is zeroes?
; It creates a tensor the same shape as the input, with a 0
; in each position. This is useful for the velocity algorithm,
; because the parameters are in a tensor, and the velocity
; for each parameter should be 0 at the start:
(display "A tensor of shape (2 3) with zeroes:")
(newline)
(zeroes (tensor (tensor 2.1 9.3 1.5)
                (tensor 7.2 3.3 6.6)))
(newline)

; The deflation function for gd with velocity
(define velocity-d
  (lambda (P)
    (list-ref P 0)))

; The update function
(define velocity-u
  (lambda (P g)
    (let ((
           v ; the velocity is...
           (- (* mu (list-ref P 1)) ; mu times previous velocity
              (* alpha g)) ; minus alpha * the gradient
           ))
      (list (+ (list-ref P 0) v) v))))

; New kind of gradient descent
(define velocity-gradient-descent
  (gradient-descent
   velocity-i velocity-d velocity-u))
