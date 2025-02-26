#lang racket
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
