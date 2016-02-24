#lang racket

;; ----------------------------------------------------------------------
;; Problem 9: Special Pythagorean triplet
;; https://projecteuler.net/problem=9

;; A Pythagorean triplet is a set of three natural numbers, a < b < c, for which:

;; a^2 + b^2 = c^2

;; For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2

;; There exists exactly one Pythagorean triplet for which 
;; a + b + c = 1000. Find the product abc.

;; Answer: 31875000
;; ----------------------------------------------------------------------

;; A few notes:
;; 1) Since a + b + c <= 1000 and a < b < c, the most a can be is 332
;; 2) Since a < b, the beginning of the range of b is a + 1
;; 3) Since b < c, the end of the range of b is (1000 - a) / 2
;; 4) The range specified by in-range is [ start, end )

(for* ([a (in-range 1 333)]
       [b (in-range (+ a 1) (/ (- 1000 a) 2))])
  (let ([c (- 1000 a b)])
    (when (= (+ (* a a) (* b b)) (* c c))
      (display (* a b c)))))



      
