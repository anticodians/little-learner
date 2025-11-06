#lang racket
(require malt)

; Data
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

; How to generate a sample...

; First we need RNG
(random 45)

; Now we can do this...
(define samples
  (lambda (n s)
    (sampled n s (list))))

(define sampled
  (lambda (n i a)
    (cond
      ((zero? i) a) ; base case
      (else ; recurisve case
       (sampled n ; n stays the same
                (sub1 i) ; count down s
                (cons (random n) a)))))) ; add a random number to the list

; Example of trefs

(trefs (tensor 5.0 2.8 4.2 2.3 7.4 1.7 8.1)
       (list 6 0 3 1))

; Example with a higher-rank tensor
(trefs (tensor (tensor 5.0 1.0 2.1)
               (tensor 2.8 3.3 7.4)
               (tensor 4.2 6.7 8.2)
               (tensor 2.3 3.4 5.1)
               (tensor 7.4 8.0 9.1)
               (tensor 1.7 3.0 2.7)
               (tensor 8.1 9.3 5.4)); tensor (7 3)
       (list 6 0 3 1)) ; b

; sampling-obj, a new kind of objective function
; that runs on random samples of the training data

; First, need a batch size
(declare-hyper batch-size)

; Then define new function
(define sampling-obj
  (lambda (expectant xs ys)
    (let ((n (tlen xs)))
          (lambda (little-theta)
            (let ((b (samples n batch-size)))
              ((expectant (trefs xs b) (trefs ys b)) little-theta))))))


; How to use it

; We still need the dashed version of gradient-descent
(define gradient-descent
  (lambda (obj theta)
    (let ((f (lambda (big-theta)
             (map (lambda (p g)
                    (- p (* alpha g)))
                  big-theta
                  (gradient-of obj big-theta)))))
    (revise f revs theta))))

(with-hypers
    ((revs 1000)
     (alpha 0.01)
     (batch-size 4))
  (gradient-descent
   (sampling-obj
    (l2-loss line) line-xs line-ys)
   (list 0.0 0.0)))

(with-hypers
    ((revs 15000)
     (alpha 0.001)
     (batch-size 4))
  (gradient-descent
   (sampling-obj
    (l2-loss plane) plane-xs plane-ys)
   (list (tensor 0.0 0.0) 0.0)))