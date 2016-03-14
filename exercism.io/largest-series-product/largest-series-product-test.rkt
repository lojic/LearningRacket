#lang racket

(require "largest-series-product.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define suite
    (test-suite
     "Tests for the largest-series-product exercise"

     (for ([tuple (in-list '(("1" 1 1)
                             ("123" 2 6)
                             ("321" 2 6)
                             ("16482932" 2 32)
                             ("0123456789" 2 72)
                             ("12" 2 2)
                             ("576802143" 2 48)
                             ("0123456789" 3 504)
                             ("1027839564" 3 270)
                             ("0123456789" 5 15120)
                             ("73167176531330624919225119674426574742355349194934" 6 23520)
                             ("52677741234314237566414902593461595376319419139427" 6 28350)
                             ("0000" 2 0)
                             ))])
          (match-define (list str n result) tuple)
          (check-equal? (largest-series-product str n) result))
     ))
     

  (run-tests suite))
