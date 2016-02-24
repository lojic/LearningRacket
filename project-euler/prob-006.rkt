#lang racket

;; ----------------------------------------------------------------------
;; Problem 6: Sum square difference
;; https://projecteuler.net/problem=6

;; The sum of the squares of the first ten natural numbers is:

;; 1^2 + 2^2 + ... + 10^2 = 385

;; The square of the sum of the first ten natural numbers is:

;; (1 + 2 + ... + 10)^2 = 55^2 = 3,025

;; Hence the difference between the sum of the squares of the first
;; ten natural numbers and the square of the sum is 3025 − 385 = 2640.

;; Find the difference between the sum of the squares of the first one
;; hundred natural numbers and the square of the sum.

;; Answer: 25164150
;; ----------------------------------------------------------------------

(define (square n) (* n n))

(define (sum-square-diff nums)
  (- (square (apply + nums))
     (apply + (map square nums))))

(display (sum-square-diff (range 1 101)))
