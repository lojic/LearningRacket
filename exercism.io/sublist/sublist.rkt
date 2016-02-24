#lang racket

(provide compare)

(define/match (compare a b)
  [(a a) 'equal]
  [(a b) (cond [(sublist? a b a b) 'sublist]
               [(sublist? b a b a) 'superlist]
               [else               'unequal])])

(define/match (sublist? a b c d)
  [ ('() _ _ _) #t ]
  [ (_ '() _ _) #f ]
  [ ((cons head a-tail) (cons head b-tail) init-a init-b)
    (sublist? a-tail b-tail init-a init-b) ]
  [ (_ _ init-a (cons _ init-b-tail))
    (sublist? init-a init-b-tail init-a init-b-tail) ])
