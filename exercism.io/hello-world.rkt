#lang racket

(provide hello)

(define (hello [name "World"])
  (format "Hello, ~a!" name))
