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
(define l2-loss-line (l2-loss line))

; Objective function for line function with given data
(display "l2-loss initial theta for line with our data:")
(newline)
(((l2-loss line) line-xs line-ys)
 (list 0.0 0.0))

(display "l2-loss with better guess")
(newline)
(((l2-loss line) line-xs line-ys)
 (list 0.0099 0.0))

(display "Could we try revising by the absolute value of the rate of change")
(newline)
(((l2-loss line) line-xs line-ys)
 (list (+ (abs -62.63) 0.0099) 0.0))

