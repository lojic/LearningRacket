#lang racket
(require "../lojic.rkt")
(require racket/date)

(let* ([ age (string->number (gets "What is your current age? ")) ]
       [ retire-age (string->number (gets "At what age would you like to retire? ")) ]
       [ years-left (- retire-age age) ]
       [ current-year (date-year (current-date)) ]
       [ retire-year (+ current-year years-left) ])
  (printf "You have ~a years left until you can retire.\n" years-left)
  (printf "It's ~a, so you can retire in ~a" current-year retire-year))



