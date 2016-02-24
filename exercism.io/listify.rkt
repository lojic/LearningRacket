#lang racket

(provide listify)

(define (listify func)
  (λ (str) (list->string (func (string->list str)))))
