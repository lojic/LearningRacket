#lang racket
(require "../lojic.rkt")

(let* ([tax-rate 0.055]
       [subtotal (get-num "What is the order amount? ")]
       [state    (gets "What is the state? ")]
       [total    (if (string=? "WI" state)
                     (let ([tax (* tax-rate subtotal)])
                       (printf "The subtotal is $~a\n" (~0.2r subtotal))
                       (printf "The tax is $~a\n" (~0.2r tax))
                       (+ subtotal tax))
                     subtotal)])
  
  (printf "The total is $~a" (~0.2r total)))

