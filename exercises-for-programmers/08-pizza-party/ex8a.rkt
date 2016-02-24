#lang racket
(require "../lojic.rkt")

(let* ([ num-people (string->number (gets "How many people? ")) ]
       [ num-pizzas (string->number (gets "How many pizzas do you have? "))]
       [ slices-per-pizza (string->number (gets "How many slices per pizza? "))]
       [ num-slices (* num-pizzas slices-per-pizza) ])
  (let-values ([(whole-slices left-slices) (quotient/remainder num-slices num-people)])
    (printf "~a people with ~a pizzas\n" num-people num-pizzas)
    (printf "Each person gets ~a pieces of pizza.\n" whole-slices)
    (printf "There are ~a leftover pieces.\n" left-slices)))




