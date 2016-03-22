#lang racket

(require "difference-of-squares.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define suite
    (test-suite
     "Tests for the difference-of-squares exercise"
     
     (test-case "square of sums to 5"
                (check-equal? (sum-square-diff 5) 170))
     (test-case "square of sums to 10"
                (check-equal? (sum-square-diff 10) 2640))
     (test-case "square of sums to 100"
                (check-equal? (sum-square-diff 100) 25164150))
     ))
  
  (run-tests suite))
  
