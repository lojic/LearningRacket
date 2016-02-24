#lang racket
(require "../lojic.rkt")

(let ([ noun (gets "Enter a noun: ")       ]
      [ verb (gets "Enter a verb: ")       ]
      [ adj  (gets "Enter an adjective: ") ]
      [ adv  (gets "Enter an adverb: ")    ])
  (printf "Do you ~a your ~a ~a ~a? That's hilarious!"
          verb adj noun adv))
