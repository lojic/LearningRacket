#lang racket

;; Modified day09-1b.rkt to make timing easier.

(define input (time (file->list "day09.txt")))

(define (validate [ lst input ])
  (let ([ x (list-ref lst 25) ])
    (if (member x (map (curry apply +) (combinations (take lst 25) 2))) (validate (cdr lst)) x)))

(time (validate))

(module+ test
  (require rackunit)
  (check-equal? (validate) 20874512))
