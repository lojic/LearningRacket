#lang racket

(require "grains.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define suite
    (test-suite
     "Tests for the grains exercise"

     (test-case "square 1"
                (check-equal? (square 1) 1))
     (test-case "square 2"
                (check-equal? (square 2) 2))
     (test-case "square 3"
                (check-equal? (square 3) 4))
     (test-case "square 4"
                (check-equal? (square 4) 8))
     (test-case "square 16"
                (check-equal? (square 16) 32768))
     (test-case "square 32"
                (check-equal? (square 32) 2147483648))
     (test-case "square 64"
                (check-equal? (square 64) 9223372036854775808))
     (test-case "total 1 squares"
                (check-equal? (total 1) 1))
     (test-case "total 2 squares"
                (check-equal? (total 2) 3))
     (test-case "total 3 squares"
                (check-equal? (total 3) 7))
     (test-case "total 64 squares"
                (check-equal? (total 64) 18446744073709551615))
     ))
     

  (run-tests suite))
