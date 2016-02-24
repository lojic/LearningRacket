#lang racket

(require rackunit)

(define (add3 n)
  (+ 3 n))

(check-equal? (add3 2) 
              4)
