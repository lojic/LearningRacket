#lang racket

(provide xmas) ; Allow use in Part 2

(define (validate n lst)
  (let ([ x (list-ref lst n) ])
    (if (member x (map (curry apply +) (combinations (take lst n) 2))) #f x)))

(define (xmas n fname)
  (let loop ([ lst (map string->number (file->lines fname)) ])
    (or (validate n lst) (loop (cdr lst)))))

(module+ test
  (require rackunit)
  (check-equal? (xmas 5 "day09-test.txt") 127)
  (check-equal? (xmas 25 "day09.txt") 20874512))
