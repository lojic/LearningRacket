#lang racket
(provide (all-defined-out))

(define (sum tup) (apply + tup))
(define (product tup) (apply * tup))
(define (pythagorean? a b c) (= (+ (* a a) (* b b)) (* c c)))

(define (generate beg end [sum 0])
  (for*/list ([a (in-range beg end)]
              [b (in-range (+ a 1) end)]
              [c (in-range (+ b 1) end)]
              #:when (and (or (= sum 0) (= sum (+ a b c)))
                          (pythagorean? a b c)))
             (list a b c)))