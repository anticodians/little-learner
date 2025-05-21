#lang racket
(require malt)

; Example
(define line-xs (vector 2.0 1.0 4.0 3.0))
(define line-ys (vector 1.8 1.2 4.2 3.3))

; First guess
(define little-theta (list 0.0 0.0))

; First guess predictions
(display "Our first guess")
(newline)
((line line-xs) little-theta)

; Loss function
(display "This gives a tensor of all the losses")
(newline)
(- line-ys ((line line-xs) little-theta))

; We can't just take the sum...
(display "This gives 0, even though the predictions aren't perfect...")
(newline)
(sum
 (tensor 4.0 -4.0 0.0 1.0 -1.0))

; l2-loss
(define l2-loss
  (lambda (target) ; the function we are trying to 'learn'
    (lambda (xs ys) ; the data: the x-values and y-values we have
      (lambda (theta) ; our current guess for the function parameters
        (let ((pred-ys ((target xs) theta)))
          (sum
           (sqr
            (- ys pred-ys))))))))

; Expectant function for line
; The expectant function knows what function
; we are trying to 'learn', but doesn't know
; the data
(define expectant-function (l2-loss line))

; Objective function for line, x and y
; The objective function knows what function
; we are tyring to 'learn', and has out training
; data available. But it does not know our latest guess
; for the function parameters.
(define objective-function (expectant-function line-xs line-ys))

; Now we can apply our objective function to
; a given guess for theta, to see how good
; our current guess is:
(display "l2-loss for our first guess for theta, (0.0 0.0):")
(newline)
(((l2-loss line) line-xs line-ys)
 (list 0.0 0.0))
(display "or, using our pre-defined `objective-function`:")
(newline)
(objective-function (list 0.0 0.0))

(display "l2-loss with better guess")
(newline)
(((l2-loss line) line-xs line-ys)
 (list 0.0099 0.0))

(display "Could we try revising by the absolute value of the rate of change?")
(newline)
(((l2-loss line) line-xs line-ys)
 (list (+ (abs -62.63) 0.0099) 0.0))

