#lang racket

(require "sum-of-multiples.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define suite
    (test-suite
     "Tests for the sum-of-multiples exercise"

     (check-equal? (sum-of-multiples 1) 0)
     (check-equal? (sum-of-multiples 4) 3)
     (check-equal? (sum-of-multiples 10) 23)
     (check-equal? (sum-of-multiples 20) 78)
     (check-equal? (sum-of-multiples 100) 2318)
     (check-equal? (sum-of-multiples 1000) 233168)
     (check-equal? (sum-of-multiples 20 '(7 13 17)) 51)
     (check-equal? (sum-of-multiples 15 '(4 6)) 30)
     (check-equal? (sum-of-multiples 150 '(5 6 8)) 4419)
     (check-equal? (sum-of-multiples 10000 '(43 47)) 2203160)
     ))
     

  (run-tests suite))
