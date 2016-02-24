#lang racket
(require "../lojic.rkt")
(require racket/date)

(define sq-feet-to-meters 0.09290304)

(let* ([ length (string->number (gets "What is the length of the room in feet? ")) ]
       [ width  (string->number (gets "What is the width of the room in feet? "))  ]
       [ area   (* length width) ]
       [ area-meters (* area sq-feet-to-meters) ])
  (printf "You entered dimensions of ~a feet by ~a feet.\n" length width)
  (printf "The area is:\n~a square feet\n~a square meeters\n" area (~0.2r area-meters)))



