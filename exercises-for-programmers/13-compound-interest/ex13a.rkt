#lang racket
(require "../lojic.rkt")

(define principal (string->number (gets "What is the principal amount? ")))
(define int-rate  (/ (string->number (gets "What is the rate? ")) 100.0))
(define num-years (string->number (gets "What is the number of years? ")))
(define num-periods
  (string->number (gets "What is the number of times the interest is compounded per year? ")))
(define amount (* principal
                  (expt (+ 1 (/ int-rate num-periods))
                        (* num-periods num-years))))

(printf "$~a invested at ~a% for ~a years compounded ~a times per year is $~a"
        (~0.2r principal) (* int-rate 100.0) num-years num-periods (~0.2r amount))
