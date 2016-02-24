#lang racket
(require "../lojic.rkt")

(define a (get-num "Enter the first number:"))
(define b (get-num "Enter the second number:"))
(define c (get-num "Enter the third number:"))
(when (and (= a b) (= b c)) (exit 0))
(printf "The largest number is ~a\n" (if (> a b)
                                         (if (> a c) a c)
                                         (if (> b c) b c)))

