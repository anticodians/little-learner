#lang racket
(require malt)

; Is this number a scalar?
(scalar? 7.1)

; A tensor_1
(tensor 5.0 7.18 3.14)

; A tensor_2
(tensor (tensor 3 2 8)
        (tensor 7 1 9))

; How many elements in that tensor?
(tlen (tensor (tensor 3 2 8)
              (tensor 7 1 9)))

(tlen (tensor 17 12 91 67))

; Is this possible?
(tensor (tensor (tensor 5) (tensor 6) (tensor 7))
        (tensor (tensor 8) (tensor 9) (tensor 0)))

; Find the rank of a tensor
; Dashed rank in frame 25
(define rank-1
  (lambda (t)
    (cond
      ((scalar? t) 0) ; base test
      (else (add1 (rank-1 (tref t 0))))))) ; recursive case

; Find the shape of a tensor
(define shape
  (lambda (t)
    (cond
      ((scalar? t) (list)) ; base case
      (else (cons (tlen t) (shape (tref t 0)))))))

; What is (shape 9)
(shape 9)

(display "What is (shape [9 4 7 8 0 1])?")
(newline)
(shape (tensor 9 4 7 8 0 1))

; Final definition of 'rank
(define rank
  (lambda (t)
    (ranked t 0)))

(define ranked
  (lambda (t a)
    (cond
      ((scalar? t) a)
      (else (ranked (tref t 0) (add1 a))))))

; Example
(rank (tensor (tensor (tensor 8) (tensor 9))
              (tensor (tensor 4) (tensor 7))))