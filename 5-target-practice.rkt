#lang racket
(require malt)

; Chapter 5

; Using the new hyperparameter syntax, we declare the names
; of our two hyperparameters: in this case, the number of training
; revisions, and the learning rate (alpha)
(declare-hyper revs)
(declare-hyper alpha)

; The dataset from Chapters 1-4
(define line-xs (vector 2.0 1.0 4.0 3.0))
(define line-ys (vector 1.8 1.2 4.2 3.3))

; Dashed version of gradient-descent – this is soon to be
; replaced, presumably.
(define gradient-descent
  (lambda (obj theta)
    (let ((f (lambda (big-theta)
             (map (lambda (p g)
                    (- p (* alpha g)))
                  big-theta
                  (gradient-of obj big-theta)))))
    (revise f revs theta))))

(display "When we use with-hypers to fit a line to line-xs and line-ys, we\nlearn the following parameters:\n")

(with-hypers
    ((revs 1000)
     (alpha 0.01))
  (gradient-descent
   ((l2-loss line) line-xs line-ys)
   (list 0.0 0.0)))

(display "Now we have a new dataset:") (newline)

(define quad-xs
  (tensor -1.0 0.0 1.0 2.0 3.0))
(define quad-ys
  (tensor 2.55 2.1 4.35 10.2 18.25))

(display "The x-values:")
quad-xs
(display "The y-values:")
quad-ys

; What is scaling?
(let ((x 2.0))
  (* 5.0 x))

; What is a quadratic function?
(define quad
  (lambda (t)
    (lambda (theta)
      (+ (* (ref theta 0) (sqr t))
         (+ (* (ref theta 1) t) (ref theta 2))))))

; The example
((quad 3.0) (list 4.5 2.1 7.8))

; Could we make it more readable?
(define ^2 sqr)
(define square sqr)
(define ^3 (lambda (x) (* x (* x x))))

(^2 3)
(^3 3)
(square 3)

; Now set up the learning problem
; target function is 'quad
(define expectant (l2-loss quad))
(define objective (expectant quad-xs quad-ys))

; Learn the parameters!
(display "The learned parameters are:") (newline)
(with-hypers
    ((revs 1000) ; number of revisions/guesses
     (alpha 0.001)) ; learning rate
    (gradient-descent
     objective
     (list 0.0 0.0 0.0)))

; Yet another dataset
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

(tlen plane-xs)
(tlen plane-ys)

; New target function
(define plane
  (lambda (t)
    (lambda (theta)
      (+ (dot-product
          (ref theta 0) ; What shape should this be?
          t ; This is a tensor-1
          )
         (ref theta 1) ; This has to be a scalar
         ))))

; What is dot-product?
(define dot-product-1-1
  (lambda (w t)
    (sum-1
     (* w t))))

; e.g.
(dot-product-1-1
 (tensor 2.0 1.0 7.0)
 (tensor 8.0 4.0 3.0))

; And just a reminder of what '* does with tensors.
; It does "pairwise" multiplication.
(* (tensor 2.0 1.0 7.0) (tensor 8.0 4.0 3.0))

; New initial guess...
(list
 (tensor 0.0 0.0) ; theta-0 <-- used in dot-prod with xs
 0.0 ; theta-1 <-- a scalar added on to the result
 )

; Learning 'plane
(with-hypers
    ((revs 1000) ; number of revisions
     (alpha 0.001)) ; learning rate
  (gradient-descent ; learning algorithm
   ((l2-loss plane) plane-xs plane-ys) ; objective function
   (list (tensor 0.0 0.0) 0.0))) ; theta = initial guess

; Test the learned parameters
((plane (tref plane-xs 3))
        (list (tensor 3.98 2.04) 5.78))

; Compare to true y value
(tref plane-ys 3)