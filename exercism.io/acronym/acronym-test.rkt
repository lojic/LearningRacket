#lang racket

(require "acronym.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define suite
    (test-suite
     "Tests for the acronym exercise"
     
     (test-case "it produces acronyms from title case"
                (check-equal? (abbreviate "Portable Network Graphics") "PNG"))
     (test-case "it produces acronyms from lower case"
                (check-equal? (abbreviate "Ruby on Rails") "ROR"))
     (test-case "it produces acronyms from inconsistent case"
                (check-equal? (abbreviate "HyperText Markup Language") "HTML"))
     (test-case "it ignores punctuation"
                (check-equal? (abbreviate "First in, First out") "FIFO"))
     (test-case "produces acronyms ignoring punctuation and casing"
                (check-equal? (abbreviate "Complementary Metal-Oxide semiconductor") "CMOS"))
     ))
  
  (run-tests suite))
  
