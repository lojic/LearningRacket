#lang racket

(require "saddle-points.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define suite
    (test-suite
     "Tests for the saddle points exercise"
     
     (test-case "no saddle point"
                (check-equal? (saddle-points "2 1\n1 2") '()))
     (test-case "a saddle point"
                (check-equal? (saddle-points "1 2\n3 4") '((0 1))))
     (test-case "another saddle point"
                (check-equal? (saddle-points "18 3 39 19 91\n38 10 8 77 320\n3 4 8 6 7") '((2 2))))
     (test-case "another saddle point from readme"
                (check-equal? (saddle-points "9 8 7\n5 3 2\n6 6 7") '((1 0))))
     (test-case "multiple saddle points"
                (check-equal? (saddle-points "4 5 4\n3 5 5\n1 5 4") '((0 1)(1 1)(2 1))))
     ))
  
  (run-tests suite))
  
