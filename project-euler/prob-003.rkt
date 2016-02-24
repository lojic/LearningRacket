#lang racket

;; ----------------------------------------------------------------------
;; Problem 3: Largest prime factor
;; https://projecteuler.net/problem=3

;; The prime factors of 13195 are 5, 7, 13 and 29.

;; What is the largest prime factor of the number 600851475143 ?

;; Answer: 6857
;; ----------------------------------------------------------------------

;; This solution utilizes the prime-divisors function provided by the
;; math/number-theory module in Racket.

(require math/number-theory)

(define (largest-prime n)
  (last (prime-divisors n)))

(display (largest-prime 600851475143))

