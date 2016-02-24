#lang racket

;; ----------------------------------------------------------------------
;; Problem 10: Summation of primes
;; https://projecteuler.net/problem=10

;; The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

;; Find the sum of all the primes below two million.

;; Answer: 142913828922
;; ----------------------------------------------------------------------

(require math/number-theory)

(define (sum-primes sum last limit)
  (let ([p (next-prime last)])
    (cond [(>= p limit) sum]
          [else (sum-primes (+ sum p) p limit)])))

(sum-primes 0 0 2000000)
