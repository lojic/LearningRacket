#lang racket

(provide leap-year?)

(define (leap-year? year)
  (define (div-by? x) (zero? (remainder year x)))
  (cond [ (div-by? 400) #t ]
        [ (div-by? 100) #f ]
        [ else (div-by? 4) ]))