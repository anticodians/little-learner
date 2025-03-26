#lang racket
; Import the malt package developed by the authors
(require malt)
(display "malt imported!")
(newline)
(newline)

; Chapter 1: The Lines Sleep Tonight

; This chapter begins our journey into deep learning by
; introducing linear equations. Put enough of these together,
; and you have ChatGPT! (almost)

(define first-attempt-at-line
  (lambda (x) ; argument
    (lambda (w b) ; parameters
      (let ((y (+ (* w x) b)))
        y))))

(define not-backwards-line
  (lambda (w b)
    (lambda (x)
      (let ((y (+ (* w x) b)))
        y))))

(define line-next ; y is now implicit 
  (lambda (x)
    (lambda (w b)
      (+ (* w x) b))))

; what is (line 8)?
; it is the 'same as'
(define same-as-line-8
  (lambda (w b)
    (+ (* w 8) b)))

; Example of 'learning'

; For this dataset...
(define line-xs
  (list 2.0 1.0 4.0 3.0))

(define line-ys
  (list 1.8 1.2 4.2 3.3))

; We could 'learn' this function:
(define predict
  (lambda (x)
    ((line x) 1 0)))

; Expressing parameters as a tensor, like theta in the text.
; (We haven't met tensors yet, but when the authors write
; [...] in the text, they mean a tensor. See page xxiii.)

; These are the parameters we 'learned':
(define learned-parameters
  (tensor 1.0 0.0))

; Here is how to get theta-zero or theta-one. 'tref is short
; for "tensor reference." In Scheme/Racket, functions called
; "ref" normally allow you to get an item out of a collection
; using its numerical index.

(define zeroth-member
  (lambda (theta)
    (tref theta 0)))

(define first-member
  (lambda (theta)
    (tref theta 1)))

; Using theta, we get the final definition of line
(define line
  (lambda (x)
    (lambda (theta)
      (+ (* (zeroth-member theta) x) ; wx
         (first-member theta)))))    ; b

; To use this one, we need to give 'line the
; w and b in a list
(display "The predicted output when x=5, w=4 and b=2:")
(newline)
((line 10.0)            ; x = 10.0
 (tensor 4.0 2.0)) ; little-theta = [4.0, 2.0]

(newline)

(display "The predicted output when x=7, and the parameters are θ as given in the text:")
(newline)
((line 7.0) learned-parameters)