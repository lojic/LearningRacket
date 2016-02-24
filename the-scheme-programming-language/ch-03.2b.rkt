#lang racket

;; Incorrect version of or macro
(define-syntax my-or
  (syntax-rules ()
    [(_) #f]
    [(_ e1 e2 ...)
     (let ([t e1])
       (if t t (my-or e2 ...)))]))
(define (foo n) 8)
(define x 1)
(my-or (= x 0) (foo 7))


