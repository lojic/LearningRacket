#lang racket

(require "nucleotide-count.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define suite
    (test-suite
     "Tests for the nucleotide-count exercise"
     
     (test-case "empty dna string has no adenosine"
                (check-equal? (count "" #\A) 0))
     (test-case "repetitive cytidine gets counted"
                (check-equal? (count "CCCCC" #\C) 5))
     (test-case "counts only thymidine"
                (check-equal? (count "GGGGGTAACCCGG" #\T) 1))
     (test-case "empty dna string has no nucleotides"
                (check-equal? (histogram "") #hash((#\A . 0) (#\T . 0) (#\C . 0) (#\G . 0))
                              ))
     (test-case "repetitive sequence has only guanosine"
                (check-equal? (histogram "GGGGGGGG") #hash((#\A . 0) (#\T . 0) (#\C . 0) (#\G . 8))))
     (test-case "counts all nucleotides"
                (define s "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC")
                (check-equal? (histogram s) #hash((#\A . 20) (#\T . 21) (#\C . 12) (#\G . 17))))
     (test-case "histogram validates the strand"
                (check-exn exn:fail:contract? (λ () (histogram "JOHNNYAPPLESEED"))))
     (test-case "count validates the nucleotide"
                (check-exn exn:fail:contract? (λ () (count "" #\Z))))
     (test-case "count validates the strand"
                (check-exn exn:fail:contract? (λ () (count "JOHNNYAPPLESEED" #\A))))
     ))
  
  (run-tests suite))
  
