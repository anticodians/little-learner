#lang racket
(require malt)

; Chapters 3 and 4

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

; We should multiply the rate of change by
; a small number to get the learning rate


; Parabola: four points:
(display "The parabola at start of Ch 4")
(newline)
(display "Loss when w == -1.0: ")
(objective-function (list -1.0 0.0))
(display "Loss when w == 0.0: ")
(objective-function (list 0.0 0.0))
(display "Loss when w == 1.0: ")
(objective-function (list 1.0 0.0))
(display "Loss when w == 2.0: ")
(objective-function (list 2.0 0.0))
(display "Loss when w == 3.0: ")
(objective-function (list 3.0 0.0))


(display "Loss when w == 0.6623: ")
(objective-function (list 0.6623 0.0))

; grandient-of example
(newline)
(display "Example on page 78")
(newline)
(gradient-of (lambda (theta) (sqr (ref theta 0))) (list 27.0))

; gradient of l2-loss
(newline)
(display "Calculating gradients for two parameters")
(newline)
(define obj ((l2-loss line) line-xs line-ys))
(gradient-of obj (list 0.0 0.0))

; iteration function, to repeatedly use
; gradient-of to improve a model
(define revise
  (lambda (f revs theta)
    (cond
      ((zero? revs) theta)
      (else
       (revise f (sub1 revs) (f theta))))))

; Example of map
(newline)
(display "Map add1 over a list of three numbers:")
(newline)
(map (lambda (x) (add1 x)) (list 5 7 3))

; More general map
(display "Or, with two lists of parameters...")
(newline)
(map (lambda (x y) (+ x y))
     (list 12 17 32)
     (list 8 3 11))

; Revise used with -3
(newline)
(display "Now an example of revise:")
(newline)
(revise (lambda (theta) ; function to revise
          (map (lambda (p) (- p 3)) ; inner function to 'map'
               theta ; data to 'map over'
               )) 
        5 ; number of revisions/improvements
        (list 3 2 1) ; initial guess to improve
        )

; Now use revise to improve our made-up linear model
(newline)
(display "The result of peforming 1000 revisions of line:")
(newline)
(let ((alpha 0.01) ; learning rate
      (obj ((l2-loss line) line-xs line-ys))) ; objective function
  (let ((f (lambda (theta) ; update function
             (let ((gs (gradient-of obj theta))) ; first get gradients
               (list ; then update each parameter using the gradients and learning rate
                (- (ref theta 0) (* alpha (ref gs 0))) ; new W
                (- (ref theta 1) (* alpha (ref gs 1))) ; new B
                )))))
    (revise f 1000 (list 0.0 0.0))))
    
; revise using map

(define revs 1000)
(define alpha 0.01)

(newline)
(let ((f (lambda (theta)
           (let ((gs (gradient-of obj theta)))
             (map (lambda (p g)
                         (- p (* alpha g)))
                  theta
                  gs)))))
  (revise f 1000 (list 0.0 0.0)))

; frame 43


(let ((obj ((l2-loss line) line-xs line-ys)))
  (let ((f (lambda (theta)
             (map (lambda (p g)
                    (- p (* alpha g)))
                  theta
                  (gradient-of obj theta)))))
    (revise f revs (list 0.0 0.0))))

; the gradient descent function

; TBC

; Hyperparameters
(declare-hyper smaller)
(declare-hyper larger)

(with-hypers
    ((smaller 1)
     (larger 2000))
  (+ smaller larger))

; nonsense function
(display "Nonsense function and 'with-hypers: ")
(define nonsense?
  (lambda (x)
    (= (sub1 x) smaller)))

(with-hypers
    ((smaller 5))
    (nonsense? 6))