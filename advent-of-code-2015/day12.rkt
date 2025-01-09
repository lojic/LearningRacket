#lang racket

(require "./advent.rkt" json)

(define input (car (parse-aoc 12)))

(define (evaluate obj)
  (match obj
    [ (? number?) obj                                          ]
    [ (? list?)   (for/sum ([ x (in-list obj) ]) (evaluate x)) ]
    [ (? hash?)   (let ([ vals (hash-values obj) ])
                    (if (member "red" vals)
                        0
                        (for/sum ([ x (in-list vals) ]) (evaluate x)))) ]
    [ _           0 ]))

(check-equal? (for/sum ([ i (in-list (numbers input)) ]) i) 111754) ; Part 1
(check-equal? (evaluate (string->jsexpr input))              65402) ; Part 2
