#lang racket
(provide triangle)

(define (invalid-lengths? a b c)
  (or (>= a (+ b c))
      (>= b (+ a c))
      (>= c (+ a b))))

(define/match (triangle a b c)
  [ (a b c) #:when (ormap negative? (list a b c)) '(error "negative side length(s)") ]
  [ (a b c) #:when (invalid-lengths? a b c)       '(error "invalid side length")     ]
  [ (a a a) '(ok equilateral) ]
  [ (_ a a) '(ok isosceles)   ]
  [ (a _ a) '(ok isosceles)   ]
  [ (a a _) '(ok isosceles)   ]
  [ (_ _ _) '(ok scalene)     ])