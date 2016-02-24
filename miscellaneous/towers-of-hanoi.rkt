#lang racket/base

;; Display the series of moves necessary to move n disks from "from" to "to"
;; using "spare" as the extra spindle
(define (move n from to spare)
  (cond ((= n 0) "done")
        (else
          (move (- n 1) from spare to)
          (displayln (list from to))
          (move (- n 1) spare to from))))

(move 3 'a 'b 'c)
; =>
;(a b)
;(a c)
;(b c)
;(a b)
;(c a)
;(c b)
;(a b)
;"done"