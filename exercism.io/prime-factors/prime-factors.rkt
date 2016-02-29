#lang racket

(provide factors-for)

(define (factors-for number)
  (let loop ([num number] [factor 2] [acc '()])
    (cond [ (= 1 num) acc ]
          [ (= 0 (remainder num factor))
            (loop (quotient num factor) factor (cons factor acc)) ]
          [ else (loop num (+ 1 factor) acc) ])))