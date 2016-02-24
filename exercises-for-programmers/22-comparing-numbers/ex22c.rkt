#lang racket
(require "../lojic.rkt")

; A different minimal version
(printf "The largest number is ~a\n"
        (apply max (map (λ (s) (get-num (format "Enter the ~a number:" s)))
                        '("first" "second" "third"))))


