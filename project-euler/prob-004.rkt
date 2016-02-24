#lang racket

;; ----------------------------------------------------------------------
;; Problem 4: Largest palindrome product
;; https://projecteuler.net/problem=4

;; A palindromic number reads the same both ways. The largest
;; palindrome made from the product of two 2-digit numbers is 9009 =
;; 91 × 99.

;; Find the largest palindrome made from the product of two 3-digit
;; numbers.

;; Answer: 906609
;; ----------------------------------------------------------------------

;; This solution iterates one variable, a, over the full range of 3
;; digit numbers (100..999), and iterates a second variable, b, over
;; the range (a..999) since x * y is the same as y * x. It relies on a
;; set! assignment to store the largest found result. It also stores
;; the factors that produce the largest result.

(require srfi/13) ; for string-reverse

;; Convert an integer to a string representation and indicate whether
;; the string is a palindrome.
(define (palindromic? n)
  (let ([str (number->string n)])
    (string=? str (string-reverse str))))

(define result 0)
(define factors '())

(for* ([a (range 100 1000)]
       [b (range a 1000)]) ; No need to iterate over full range
  (let ([p (* a b)])
    (when (and (> p result) (palindromic? p))
      (set! result p)
      (set! factors (list a b)))))

(display (cons result factors)) ; => (906609 913 993)
