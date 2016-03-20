#lang racket
(provide async-task)

(define (async-task module-path fun)
  ; 1) Create queue to send work to workers and retrieve results
  (define-values (parent child) (place-channel))

  ; 2) Create one place per core
  (for ([_ (in-range (processor-count))])
         (place-channel-put (dynamic-place module-path fun) child))

  ; 3) Return queue to caller
  parent)
