#lang racket
; NOTE: modified get-num in lojic.rkt to satisfy the constraint for this exercise
(require "../lojic.rkt")

(let* ([ height (get-num "What is your height (in.)?")  ]
       [ weight (get-num "What is your weight (lbs.)?") ]
       [ bmi (* (/ weight (* height height)) 703.0)     ])
  (printf "Your BMI is ~a\n" (~0.3r bmi))
  (cond [ (< bmi 18.5) (displayln "You are underweight. See a doctor") ]
        [ (<= bmi 25)  (displayln "Your weight is normal")             ]
        [ else         (displayln "You are overweight. See a doctor")  ]))
