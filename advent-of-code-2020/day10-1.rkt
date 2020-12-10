#lang racket

(define (get-input fname) (sort (file->list fname) <))

(define (run lst)
  (let loop ([ lst lst ][ dist (hash) ][ last 0 ])
    (if (null? lst)
        (hash-update dist 3 add1 0)
        (let ([ x (car lst) ])
        (loop (cdr lst) (hash-update dist (- x last) add1 0) x)))))

(define (answer fname)
  (let ([ hsh (run (get-input "day10.txt")) ])
    (* (hash-ref hsh 1) (hash-ref hsh 3))))

(module+ test
  (require rackunit)

  (check-equal? (run (get-input "day10-test.txt")) (hash 1 7 3 5))
  (check-equal? (run (get-input "day10-test2.txt")) (hash 1 22 3 10))
  (check-equal? (run (get-input "day10.txt")) (hash 1 75 3 40))
  (check-equal? (answer "day10.txt") 3000))
