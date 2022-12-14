#lang racket

(provide render)

(define (render cave source bounds)
  (let-values ([ (left right bottom) (bounds) ])
    (for ([ y (inclusive-range 0 bottom) ])
      (for ([ x (inclusive-range left right) ])
        (let* ([ p (make-rectangular x y) ]
               [ c (hash-ref cave p 'air) ])
          (printf "~a" (if (= p source)
                           #\+
                           (match c
                             [ 'air  #\. ]
                             [ 'rock #\# ]
                             [ 'sand #\O ])))))
      (printf "\n"))
    (printf "\n")))
