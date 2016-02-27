#lang racket

(require "binary.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define suite
    (test-suite
     "Tests for the binary exercise"

     (for ([(str num) #hash(("" . 0)
                            ("0" . 0)
                            ("1" . 1)
                            ("10" . 2)
                            ("11" . 3)
                            ("100" . 4)
                            ("101" . 5)
                            ("110" . 6)
                            ("111" . 7)
                            ("foo" . 0))])
          (check-equal? (to-decimal str) num))
     ))
  
  (run-tests suite))
  
