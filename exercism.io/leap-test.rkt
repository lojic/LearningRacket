#lang racket

(require "leap.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define suite
    (test-suite
     "Tests for the leap exercise"

     (test-case "vanilla leap year"
                (check-true (leap-year? 1996)))
     (test-case "any old year"
                (check-false (leap-year? 1997)))
     (test-case "century"
                (check-false (leap-year? 1900)))
     (test-case "exceptional century"
                (check-true (leap-year? 2400)))
     ))
     

  (run-tests suite))
