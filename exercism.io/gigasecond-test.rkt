#lang racket

(require (prefix-in g: "gigasecond.rkt"))

(module+ test
  (require rackunit rackunit/text-ui)

  (define suite
    (test-suite
     "Tests for the gigasecond exercise"

     (test-case "4/25/2011"
                (check-equal? (g:from 2011 4 25) '(2043 1 1)))
     (test-case "6/13/1977"
                (check-equal? (g:from 1977 6 13) '(2009 2 19)))
     (test-case "7/19/1959"
                (check-equal? (g:from 1959 7 19) '(1991 3 27)))
     ))
     

  (run-tests suite))
