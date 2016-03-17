#lang racket

(require "palindrome-products.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define suite
    (test-suite
     "Tests for the palindrome-products exercise"

     (for ([tuple (in-list '((10 9 (1 9))
                             (100 9009 (91 99))
                             (1000 906609 (913 993))
                             ))])
          (match-define (list limit largest factors) tuple)
          (define-values (result lst) (largest-palindrome-product limit))
          (check-equal? result largest)
          (check-equal? factors lst))
     ))
     

  (run-tests suite))
