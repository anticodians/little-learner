#lang racket
(require malt)

; Shape of this
(shape (tensor (tensor 2 4 5)
               (tensor 6 7 9))) ; '(2 3)

; A common kind of tensor, a 'column matrix'
(tensor (tensor 5)
        (tensor 7)
        (tensor 8))

; How to write [[5 7 8]]? (a 'row matrix')
(tensor (tensor 5 7 8))
; Its shape is '(1 3)
(shape (tensor (tensor 5 7 8)))

; NB, a row matrix is not a tensor-1
(tensor 5 7 8)
; Its shape is different
(shape (tensor 5 7 8))
