#lang racket
(require malt)

; If we wanted to sample 5 items from a dataset of 20...
(samples
 20 ; n = number of points in dataset
 5 ; s = number of points to sample
 )

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