#lang racket

(require "nth-prime.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define suite
    (test-suite
     "Tests for the nth-prime exercise"
     
     (test-case "first prime"
                (check-equal? (nth-prime 1) 2))
     (test-case "second prime"
                (check-equal? (nth-prime 2) 3))
     (test-case "third prime"
                (check-equal? (nth-prime 3) 5))
     (test-case "fourth prime"
                (check-equal? (nth-prime 4) 7))
     (test-case "fifth prime"
                (check-equal? (nth-prime 5) 11))
     (test-case "sixth prime"
                (check-equal? (nth-prime 6) 13))
     (test-case "100th prime"
                (check-equal? (nth-prime 100) 541))
     (test-case "500th prime"
                (check-equal? (nth-prime 500) 3571))
     ))
  
  (run-tests suite))
  
