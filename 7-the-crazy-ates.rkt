#lang racket
(require malt)

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

; Example of the "lonely representation"
(display "The lonely representation: ")
(map (lambda (p)
       (list p))
     (list 7 8 9)) ; little-theta
(newline)

; How to 'inflate' little-theta into big-theta
(define lonely-i
  (lambda (little-theta)
    (map (lambda (p)
           (list p))
         little-theta)))

(display "You can create the lonely representation using 'lonely-i:")
(newline)
(lonely-i (list 7 8 9))
(newline)

; How to 'deflate' the lonely representation back
; into a simple list of parameters
(define lonely-d
  (lambda (big-theta)
    (map (lambda (big-p) ; P = accompanied
           (ref big-p 0))
         big-theta)))

(display "The deflate function removes the nested lists:")
(newline)
(lonely-d (lonely-i (list 7 8 9)))
(newline)

; Function for revising big-theta
(define lonely-u
  (lambda (big-theta gs)
    (map (lambda (big-p g)
           (list (- (ref big-p 0) (* alpha g))))
         big-theta
         gs)))

; lonely-u makes sure that after each iteration,
; big-theta is still in the correct 'inflated' format


; Dashed gradient-descent
(define gradient-descent
  (lambda (inflate deflate update)
   (lambda (obj little-theta)
    (let ((f (lambda (big-theta)
               (update
                big-theta
                (gradient-of obj
                             (deflate big-theta))))))
      (deflate
        (revise f revs
                (inflate little-theta)))))))

; A particular gradient descent function
(define lonely-gradient-descent
  (gradient-descent
   lonely-i lonely-d lonely-u))

; An example, which will let us experiment
; with different values for 'inflate 'deflate and 'update
(define try-plane
  (lambda (a-gradient-descent)
    (with-hypers
        ((revs 15000)
         (alpha 0.001)
         (batch-size 4))
      (a-gradient-descent
       (sampling-obj
        (l2-loss plane) plane-xs plane-ys)
       (list (tensor 0.0 0.0) 0.0)))))

; Try out a particular version of gradient-descent
(display "The 'lonely' version of gradient descent can fit the 'plane' data:")
(newline)
(try-plane lonely-gradient-descent)
(newline)

; The "naked representation"
(define naked-i
  (lambda (little-theta)
    (map (lambda (p)
           (let ((P p))
             P))
         little-theta)))

(define naked-d
  (lambda (big-theta)
    (map (lambda (P)
           (let ((p P))
             p))
         big-theta)))

(define naked-u
  (lambda (big-theta gs)
    (map (lambda (P g)
           (- P (* alpha g)))
         big-theta
         gs)))

(define naked-gradient-descent
  (gradient-descent
   naked-i naked-d naked-u))

; Now run gradient descent using this
; representation of the parameters
(display "The 'naked' representation produces a similar set of parameters:")
(newline)
(try-plane naked-gradient-descent)
(newline)

; Now let's abstract out the 'inflation'
; and 'deflation' functions, and create the
; final definition of gradient-descent
(define gradient-descent-final
  (lambda (inflate deflate update)
    (lambda (obj little-theta)
      (let ((f (lambda (big-theta)
                 (map update
                      big-theta
                      (gradient-of obj
                                   (map deflate big-theta))))))
        (map deflate
             (revise f revs
                     (map inflate little-theta)))))))

; Lonely representation in more compact form
; In this new form, you just need to specify
; what to do with each parameter – 'gradient-descent
; takes care of looping over each parameter and
; applying the operation.
(define lonely-i-concise
  (lambda (p) (list p)))
(define lonely-d-concise
  (lambda (P) (list-ref P 0)))
(define lonely-u-concise
  (lambda (P g) (list (- (list-ref P 0) (* alpha g)))))

; It works!
(define lonely-gradient-descent-final
  (gradient-descent-final
   lonely-i-concise lonely-d-concise lonely-u-concise))

(display "The 'ultimate' implementation of gradient descent")
(newline)
(display "takes care of maping the inflation, deflation and")
(newline)
(display "update functions for us. The results are, of course, identical:")
(newline)
(try-plane lonely-gradient-descent-final)