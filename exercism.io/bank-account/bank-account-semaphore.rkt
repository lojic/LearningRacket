#lang racket
(require "../concurrency.rkt")
(provide open-bank balance update)

(define/atomic (open-bank)
  (box 0))

(define/atomic (balance account)
  (unbox account))

(define/atomic (update account amount)
  (set-box! account (+ amount (unbox account)))
  (unbox account))