#lang racket

(require "scrabble-score.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define suite
    (test-suite
     "Tests for the scrabble-score exercise"

     (test-case "empty word scores zero"
                (check-equal? (score "") 0))
     (test-case "whitespace scores zero"
                (check-equal? (score " \t\n") 0))
     (test-case "scores very short word"
                (check-equal? (score "A") 1))
     (test-case "scores other very short word"
                (check-equal? (score "f") 4))
     (test-case "simple word scores the number of letters"
                (check-equal? (score "street") 6))
     (test-case "complicated word scores more"
                (check-equal? (score "quirky") 22))
     (test-case "scores are case insensitive"
                (check-equal? (score "MultiBILLIONAIRE") 20))
     (test-case "convenient scoring"
                (check-equal? (score "alacrity") 13))
     ))
     

  (run-tests suite))
