#lang racket
(require "../lojic.rkt")

; Each element in the states list is of the form:
; (name state-rate counties)
; Each element in counties is of the form:
; (name county-rate)
(define states '(("Wisconsin" 0.0 (("Eau Claire" 0.05)
                                   ("Dunn" 0.04)))
                 ("Illinois" 0.08 ())))

; Given a list of counties, return the tax-rate for the county. If the list is empty, simply return 0;
; otherwise, prompt the user for their county and return the associated county rate.
(define (county-rate counties)
  (if (null? counties)
      0.0
      (cadr (assoc (gets "What county do you live in?") counties))))

; Given an element from the states list, return the associated tax rate as the sum of the state-rate
; plus any county-rate that may exist.
(define (tax-rate state-element)
  (let ([state-rate (cadr state-element)]
        [counties (caddr state-element)])
    (+ state-rate (county-rate counties))))

(define (calculate-tax amount state)
  (define state-element (findf (λ (lst) (string=? state (car lst))) states))
  (if state-element
      (* amount (tax-rate state-element))
      0.0))

(define amount (get-num "What is the order amount?"))
(define state (gets "What state do you live in?"))
(define tax (calculate-tax amount state))

(printf "The tax is $~a\n" (~0.2r tax))
(printf "The total is $~a\n" (~0.2r (+ amount tax)))
