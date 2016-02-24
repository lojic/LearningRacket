#lang racket

;; ----------------------------------------------------------------------
;; Problem 1: Multiples of 3 and 5
;; https://projecteuler.net/problem=1

;; If we list all the natural numbers below 10 that are multiples of 3
;; or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

;; Find the sum of all the multiples of 3 or 5 below 1000.

;; Answer: 233168
;; ----------------------------------------------------------------------

;; Rather than check each number to see if it's a multiple of 3 or 5,
;; this solution does the following:
;; 1) Sum all the multiples of 3 less than the limit
;; 2) Sum all the multiples of 5 less than the limit and are not also
;;    multiples of 3

(define (sum-3-5 limit)
  ; Sum all multiples of 3 which are less than the limit
  (define (sum-3 n accum)
    (if (>= n limit) 
        accum
        (sum-3 (+ 3 n) (+ n accum))))
  ; Sum all multiples of 5 which are both less than
  ; the limit and not a multiple of 3
  (define (sum-5 n accum)
    (if (>= n limit)
        accum
        (sum-5 (+ 5 n) 
               (+ (if (= 0 (remainder n 3)) 0 n) 
                  accum))))
  (+ (sum-3 3 0)
     (sum-5 5 0)))
  
(sum-3-5 1000)
