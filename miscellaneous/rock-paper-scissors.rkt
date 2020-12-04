#lang racket
(require racket/contract)

(define (valid-symbol? arg)
  (member arg '(paper rock scissors)))
  
(define/contract (play a b)
  (-> valid-symbol? valid-symbol? any)
  (match (list a b)
    [ (list 'paper 'rock)     "Paper wins"     ]
    [ (list 'paper 'scissors) "Scissors wins"  ]
    [ (list 'rock  'scissors) "Rock wins"      ]
    [ (list a a)              "Tie, try again" ]
    [ _ (play b a)                             ]))