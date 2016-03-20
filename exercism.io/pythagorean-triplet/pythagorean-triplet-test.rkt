#lang racket

(require "pythagorean-triplet.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define suite
    (test-suite
     "Tests for thepythagorean-triplet exercise"

     (test-case "sum"             (check-equal? (sum '(3 4 5)) 12))
     (test-case "product"         (check-equal? (product '(3 4 5)) 60))
     (test-case "pythagorean"     (check-true (pythagorean? 3 4 5)))
     (test-case "not pythagorean" (check-false (pythagorean? 5 6 7)))
     (test-case "triplets up to 10"
                (define triplets (generate 1 11))
                (check-equal? (map product triplets) '(60 480)))
     (test-case "triplets 11 to 20"
                (define triplets (generate 11 21))
                (check-equal? (map product triplets) '(3840)))
     (test-case "triplets where sum is 180 and max factor is 100"
                (define triplets (generate 1 101 180))
                (check-equal? (map product triplets) '(118080 168480 202500)))
     ))
  
  (run-tests suite))
  
