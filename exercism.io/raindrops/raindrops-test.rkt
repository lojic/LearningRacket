#lang racket

(require "raindrops.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define suite
    (test-suite
     "Tests for the raindrops exercise"

     (for ([(num str) #hash((1 . "1")
                            (3 . "Pling")
                            (5 . "Plang")
                            (7 . "Plong")
                            (6 . "Pling")
                            (9 . "Pling")
                            (10 . "Plang")
                            (14 . "Plong")
                            (15 . "Pling Plang")
                            (21 . "Pling Plong")
                            (25 . "Plang")
                            (35 . "Plang Plong")
                            (49 . "Plong")
                            (52 . "52")
                            (105 . "Pling Plang Plong")
                            (12121 . "12121")
                            )])
          (check-equal? (convert num) str))
     ))
  
  (run-tests suite))
  
