#lang racket

(require "point-mutations.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define suite
    (test-suite
     "Tests for the point-mutation exercise"

     (test-case "no difference between empty strands"
                (check-equal? (hamming-distance "" "") 0))
     (test-case "no difference between identical strands"
                (check-equal? (hamming-distance "GGACTGA" "GGACTGA") 0))
     (test-case "small hamming distance in middle somewhere"
                (check-equal? (hamming-distance "GGACG" "GGTCG") 1))
     (test-case "larger distance"
                (check-equal? (hamming-distance "ACCAGGG" "ACTATGG") 2))
     (test-case "hamming distance is undefined for strands of different lengths"
                (check-equal? (hamming-distance "AAAC" "TAGGGGAGGCTAGCGGTAGGAC") '())
                (check-equal? (hamming-distance "GACTACGGACAGGACACC" "GACATCGC") '()))
     ))
     

  (run-tests suite))
