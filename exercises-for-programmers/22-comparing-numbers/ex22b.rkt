#lang racket
(require "../lojic.rkt")

; A more minimal version
(printf "The largest number is ~a\n" (max (get-num "Enter the first number:")
                                          (get-num "Enter the second number:")
                                          (get-num "Enter the third number:")))


