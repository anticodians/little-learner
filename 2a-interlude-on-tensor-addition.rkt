#lang racket
(require malt)

(newline)
(display "What is 1 + 1?")
(newline)
(+ 1 1)

(newline)
(display "What is (+ [2] [7])")
(newline)
(+ (tensor 2) (tensor 7))

(newline)
(display "What is the more complex sum in frame 10?")
(newline)
(+ (tensor (tensor 4 6 7)
           (tensor 2 0 1))
   (tensor (tensor 1 2 2)
           (tensor 6 3 1)))

(newline)
(display "What is 4 + [3 6 5]?")
(newline)
(+ 4 (tensor 3 6 5))

(newline)
(display "What is [6 9 1] + [[4 3 8] [7 4 7]]?")
(newline)
(+ (tensor 6 9 1)
   (tensor (tensor 4 3 8)
           (tensor 7 4 7)))

(newline)
(display "What is [[4 6 5] [6 9 7]] * 3")
(newline)
(* (tensor (tensor 4 6 5)
           (tensor 6 9 7))
   3)

(newline)
(display "What is the square root of 9?")
(newline)
(sqrt 9)

(newline)
(display "What is the square root of [9 16 25]?")
(newline)
(sqrt (tensor 9 16 25))

(newline)
(display "What is the square root of:") (newline)
(display " [[49 81 16]") (newline)
(display "  [64 25 36]]?") (newline)
(newline)
(sqrt (tensor (tensor 49 81 16)
              (tensor 64 25 36)))

(newline)
(display "Define sum^1")
(newline)
(define sum^1
  (lambda (t)
    (summed t (sub1 (tlen t)) 0.0)))

(define summed
  (lambda (t i a)
    (cond
      ((zero? i) (+ (tref t 0) a)) ; base
      (else
       (summed t (sub1 i) (+ (tref t i) a)))))) ; recursion

(newline)
(display "What is the sum of this cube of numbers")
(display "(i.e. this tensor^3)") (newline)
(display " [[[1 2] [3 4]]") (newline)
(display "  [[5 6] [7 8]]]?") (newline)
(newline)
; NB: malt provides a 'sum' function for this
; because the usual '+' doesn't work
(sum (tensor (tensor (tensor 1 2)
                     (tensor 3 4))
             (tensor (tensor 5 6)
                     (tensor 7 8))))
