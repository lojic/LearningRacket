#lang racket
(require "../lojic.rkt")

(define principal (string->number (gets "Enter the principal: ")))
(define int-rate  (/ (string->number (gets "Enter the rate of interest: ")) 100.0))
(define num-years (string->number (gets "Enter the number of years: ")))
(define amount (* principal
                  (+ 1
                     (* int-rate num-years))))
(printf "After ~a years at ~a%, the investment will be worth $~a"
        num-years (* int-rate 100.0) (~0.2r amount))
