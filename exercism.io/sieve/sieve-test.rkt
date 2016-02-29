#lang racket

(require "sieve.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define suite
    (test-suite
     "Tests for the sieve exercise"

     (for ([(limit primes) #hash((2 . (2))
                            (3 . (3 2))
                            (4 . (3 2))
                            (5 . (5 3 2))
                            (6 . (5 3 2))
                            (7 . (7 5 3 2))
                            (8 . (7 5 3 2))
                            (9 . (7 5 3 2))
                            (10 . (7 5 3 2))
                            (11 . (11 7 5 3 2))
                            (22 . (19 17 13 11 7 5 3 2))
                            )])
          (check-equal? (primes-to limit) primes))
     ))
  
  (run-tests suite))
  
