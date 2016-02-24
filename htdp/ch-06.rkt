#lang racket
(require lang/posn)
(require htdp/draw)

(define (distance-to-0 a-posn)
  (sqrt (+ (sqr (posn-x a-posn))
           (sqr (posn-y a-posn)))))


