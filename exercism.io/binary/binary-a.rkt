#lang racket
(provide to-decimal)

(define (to-decimal str)
  (let loop ([lst (string->list str)] [acc 0])
    (cond [ (null? lst)  acc                                        ]
          [ (char=? #\0 (car lst)) (loop (cdr lst) (* 2 acc))       ]
          [ (char=? #\1 (car lst)) (loop (cdr lst) (+ (* 2 acc) 1)) ]
          [ else        0                                           ])))