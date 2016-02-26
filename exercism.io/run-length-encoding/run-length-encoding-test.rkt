#lang racket

(require "run-length-encoding.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define suite
    (test-suite
     "Tests for the run-length-encoding exercise"

     (for ([(before after) #hash(("" . "")
                                 ("A" . "1A")
                                 ("AA" . "2A")
                                 ("AB" . "1A1B")
                                 ("AAB" . "2A1B")
                                 ("ABB" . "1A2B")
                                 ("ABBC" . "1A2B1C")
                                 ("AABCC" . "2A1B2C"))])
          (check-equal? (encode before) after))

     (for ([s '("" "A" "AA" "AB" "AAB" "ABB" "ABBC" "AABCC")])
          (check-equal? (decode (encode s)) s))))
  
  (run-tests suite))
  
