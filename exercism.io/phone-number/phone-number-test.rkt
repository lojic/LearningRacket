#lang racket

(require "phone-number.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define suite
    (test-suite
     "Tests for the phone-number exercise"

     (test-case "cleans number"
                (check-equal? (phone-number "(123) 456-7890") "1234567890"))
     (test-case "cleans number with dots"
                 (check-equal? (phone-number "123.456.7890") "1234567890"))
     (test-case "valid when 11 digits and first is 1"
                (check-equal? (phone-number "11234567890") "1234567890")
                (check-equal? (phone-number "+1 (303) 555-1212") "3035551212"))
     (test-case "invalid when 11 digits"
                 (check-equal? (phone-number "21234567890") bad-number))
     (test-case "invalid when 9 digits"
                 (check-equal? (phone-number "123456789") bad-number))
     (test-case "invalid when proper number of digits but letters mixed in"
                 (check-equal? (phone-number "1a2a3a4a5a6a7a8a9a0a") bad-number))
     ))

  (run-tests suite))