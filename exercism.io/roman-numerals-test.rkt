#lang racket

(require "roman-numerals.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define suite
    (test-suite
     "Tests for the roman-numerals exercise"

     (test-case "the numerals"
                (check-equal? (numerals 1) "I")
                (check-equal? (numerals 2) "II")
                (check-equal? (numerals 3) "III")
                (check-equal? (numerals 4) "IV")
                (check-equal? (numerals 5) "V")
                (check-equal? (numerals 6) "VI")
                (check-equal? (numerals 7) "VII")
                (check-equal? (numerals 8) "VIII")
                (check-equal? (numerals 9) "IX")
                (check-equal? (numerals 10) "X")
                (check-equal? (numerals 27) "XXVII")
                (check-equal? (numerals 48) "XLVIII")
                (check-equal? (numerals 59) "LIX")
                (check-equal? (numerals 93) "XCIII")
                (check-equal? (numerals 141) "CXLI")
                (check-equal? (numerals 163) "CLXIII")
                (check-equal? (numerals 402) "CDII")
                (check-equal? (numerals 575) "DLXXV")
                (check-equal? (numerals 911) "CMXI")
                (check-equal? (numerals 1024) "MXXIV")
                (check-equal? (numerals 3000) "MMM"))
     ))
     

  (run-tests suite))
