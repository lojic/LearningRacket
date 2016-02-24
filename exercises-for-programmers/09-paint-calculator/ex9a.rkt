#lang racket
(require "../lojic.rkt")

(define sq-feet-per-gallon 350)

(let* ([ length (string->number (gets "How long? ")) ]
       [ width (string->number (gets "How wide? "))]
       [ area (* length width) ]
       [ gallons (ceiling (/ area sq-feet-per-gallon)) ])
  (printf "You will need to purchase ~a gallons of paint to cover ~a square feet.\n"
          gallons area))




