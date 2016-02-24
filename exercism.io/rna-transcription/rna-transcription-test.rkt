#lang racket

(require "rna-transcription.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define suite
    (test-suite
     "Tests for the rna-transcription exercise"

     (test-case "transcribes guanine to cytosine"
                (check-equal? (to-rna "G") "C"))
     (test-case "transcribes cytosine to guanine"
                (check-equal? (to-rna "C") "G"))
     (test-case "transcribes thymidine to adenine"
                (check-equal? (to-rna "T") "A"))
     (test-case "transcribes adenine to uracil"
                (check-equal? (to-rna "A") "U"))
     (test-case "it transcribes all dna nucleotides to rna equivalents"
                (check-equal? (to-rna "ACGTGGTCTTAA") "UGCACCAGAAUU"))
     ))
     

  (run-tests suite))
