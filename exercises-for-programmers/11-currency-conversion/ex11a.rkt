#lang racket
(require "../lojic.rkt")

(define (cents x) (inexact->exact (floor (+ (* x 100.0) 0.5))))
(define (dollars x) (/ x 100.0))
(define euros (cents (string->number (gets "How many euros are you exchanging? "))))
(define exchange-rate (string->number (gets "What is the exchange rate? ")))
(define us-dollars (cents (* (dollars euros) exchange-rate)))

(printf "~a euros at an exchange rate of ~a is ~a U.S. dollars."
        (~0.2r (dollars euros))
        exchange-rate
        (~0.2r (dollars us-dollars)))
