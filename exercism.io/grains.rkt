#lang racket

(provide square total)

(define (square n)
  (arithmetic-shift 1 (- n 1)))

; The sum of r^k as k = 0 .. n is (1 - r^(n+1)) / (1 - r)
; So, with r = 2, we have: (1 - 2^(n+1)) / (1 - 2)
; Which simplifies to: 2^(n+1) - 1
(define (total n)
  (- (arithmetic-shift 1 n) 1))
