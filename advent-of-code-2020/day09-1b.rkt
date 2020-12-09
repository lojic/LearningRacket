#lang racket

;; I saw another Racket solution that showed me I could collapse my
;; two functions elegantly into one.

(define (validate [ lst (map string->number (file->lines "day09.txt")) ])
  (let ([ x (list-ref lst 25) ])
    (if (member x (map (curry apply +) (combinations (take lst 25) 2))) (validate (cdr lst)) x)))

(module+ test
  (require rackunit)
  (check-equal? (validate) 20874512))
