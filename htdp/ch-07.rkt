#lang racket
(require lang/posn)

(define (area-of-disk r)
  (* 3.14 (* r r)))

(define (checked-area-of-disk v)
  (cond [(number? v) (area-of-disk v)]
        [else (error 'checked-area-of-disk "number expected")]))

;; A little advanced for this part of the book :)
(define (check-function test fun)
  (lambda (v)
    (cond [(test v) (fun v)]
          [else (error 'check-function "test failed")])))
