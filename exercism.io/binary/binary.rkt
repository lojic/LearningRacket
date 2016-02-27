#lang racket
(provide to-decimal)

(define (to-decimal str)
  (let loop ([lst (string->list str)] [acc 0])
    (match lst [ '()          acc                              ]
               [ (cons #\0 _) (loop (cdr lst) (* 2 acc))       ]
               [ (cons #\1 _) (loop (cdr lst) (+ (* 2 acc) 1)) ]
               [ _            0                                ])))
          