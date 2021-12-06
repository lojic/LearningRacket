#lang racket

(require "../../advent/advent.rkt" threading)

(define input (~> (file->string "day06.txt")
                  string-trim
                  (string-split ",")
                  (map string->number _)))

(define (solve n fish)
  (define (spawn fish)
    (cond [ (null? fish) '() ]
          [ else (let ([ n (car fish) ])
                   (if (= n 0)
                       (cons 6 (cons 8 (spawn (cdr fish))))
                       (cons (sub1 n) (spawn (cdr fish))))) ]))

  (length (iterate spawn fish n)))

;; Tests --------------------------------------------------------------------------------------

(module+ test
  (require rackunit)
  (check-equal? (solve 80 input) 371379))
