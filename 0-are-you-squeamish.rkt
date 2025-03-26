#lang racket

; Chapter 0: Are you Squeamish?

; This chapter contains an *extremely* concise introduction
; to Racket and recursive functions.

(define pie 3.14)

(define a-radius 8.4)

(define an-area
  (* pie
     (* a-radius a-radius)))

(define compute-area-of-circle
  (lambda (r)
    (* pie
       (* r r))))

(define area-of-rectangle
  (lambda (width)
    (lambda (height)
      (* width height))))

(define double-result-of-f
  (lambda (f)
    (lambda (z)
      (* 2 (f z)))))

(define add3
  (lambda (x)
    (+ 3 x)))

(define abs
  (lambda (x)
    (cond
      ((< x 0) (- 0 x))
      (else x))))

(define silly-abs
  (lambda (x)
    (let ((x-is-negative (< x 0)))
      (cond
        (x-is-negative (- 0 x))
        (else x)))))

(define remainder
  (lambda (x y)
    (cond
      ((< x y) x) ; base case
      (else       ; recursive case
       (remainder (- x y) y)))))

(define add
  (lambda (n m)
    (cond
      ((zero? m) n) ; base case
      (else (add1 ; wrapper
             (add n (sub1 m)) ; 'recursive invocation'
             )))))