#lang racket
(require "../lojic.rkt")

(define max-bac 0.079999)

(let ([ weight     (get-num "What is your weight (lbs.)?")            ]
      [ gender     (gets "What is your gender (m or f)?")             ]
      [ num-drinks (get-num "How many drinks?")                       ]
      [ drink-vol  (get-num "How much alcohol in each drink (oz.)?")  ]
      [ hours      (get-num "How many hours since last drink?")       ])
  (define alcohol (* num-drinks drink-vol))
  (define ratio (if (string=? gender "m") 0.73 0.66))
  (define bac (- (/ (* alcohol 5.14)
                    (* weight ratio))
                 (* 0.015 hours)))
  (printf "Your BAC is ~a\n" (~0.3r bac))
  (displayln (if (> bac max-bac)
                 "It is not legal for you to drive."
                 "")))