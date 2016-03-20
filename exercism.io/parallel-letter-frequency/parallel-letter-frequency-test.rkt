#lang racket

; I had trouble running this from w/in DrRacket, so I tested from the commandline via:
; raco test parallel-letter-frequency-test.rkt
(require "parallel-letter-frequency.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define alphabet "abcdefghijklmnopqrstuvwxyz")
  (define alpha-hash (make-immutable-hash (map (λ (c) (cons c 1)) (string->list alphabet))))
  (define alpha-hash-10 (make-immutable-hash (map (λ (c) (cons c 10)) (string->list alphabet))))
  
  (define suite
    (test-suite
     "Tests for the parallel-letter-frequency exercise"

     (check-equal? (frequency '("a")) #hash((#\a . 1)))
     (check-equal? (frequency '("a" "b" "c" "d" "e"))
                   #hash((#\a . 1) (#\b . 1) (#\c . 1) (#\d . 1) (#\e . 1)))
     (time (check-equal? (frequency (for/list ([_ (in-range 10)])
                                        alphabet)) alpha-hash-10))
     ))
     

  (run-tests suite))
