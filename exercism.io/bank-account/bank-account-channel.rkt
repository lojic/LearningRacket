#lang racket
(require "../concurrency.rkt")
(provide open-bank balance update)

(define/atomchan (open-bank)
  (box 0))

(define/atomchan (balance account)
  (unbox account))

(define/atomchan (update account amount)
  (set-box! account (+ amount (unbox account)))
  (unbox account))