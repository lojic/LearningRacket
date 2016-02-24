#lang racket

;; ----------------------------------------------------------------------
;; Problem 7: 10001st prime
;; https://projecteuler.net/problem=7

;; By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we
;; can see that the 6th prime is 13.

;; What is the 10,001st prime number?

;; Answer: 104743
;; ----------------------------------------------------------------------

;; Easiest way - use built-in
;(require math/number-theory)
;(display (nth-prime 10000))

;; Harder way - use built-in prime? only
;(require math/number-theory) ; for prime?
;(define (my-nth-prime n)
;  (let func ([cnt 1] [i 3] [primes '(2)])
;    (if (= cnt n)
;        (car primes)
;        (if (prime? i)
;            (func (+ cnt 1) (+ i 2) (cons i primes))
;            (func cnt (+ i 2) primes)))))

;; Hardest way - everything manual
(define (is-prime? n primes)
  (not (ormap (λ (p) (= 0 (remainder n p))) (reverse primes))))

(define (my-nth-prime n)
  (let func ([cnt 1] [i 3] [primes '(2)])
    (if (= cnt n)
        (car primes)
        (if (is-prime? i primes)
            (func (+ cnt 1) (+ i 2) (cons i primes))
            (func cnt (+ i 2) primes)))))

(display (my-nth-prime 10001))