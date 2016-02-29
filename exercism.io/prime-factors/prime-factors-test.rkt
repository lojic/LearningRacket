#lang racket

(require "prime-factors.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define suite
    (test-suite
     "Tests for the prime-factors exercise"

     (for ([(num factors) #hash((1 . ())
                            (2 . (2))
                            (3 . (3))
                            (4 . (2 2))
                            (5 . (5))
                            (6 . (3 2))
                            (7 . (7))
                            (8 . (2 2 2))
                            (9 . (3 3))
                            (27 . (3 3 3))
                            (625 . (5 5 5 5))
                            (901255 . (461 23 17 5))
                            (93819012551 . (894119 9539 11))
                            (15485863 . (15485863))
                            )])
          (check-equal? (factors-for num) factors))
     ))
  
  (run-tests suite))
  
