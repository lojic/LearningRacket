#lang racket/base

; Constraint: Keep the input, string concatenation, and output separate.
(display "What is your name? ")
(let* ([name (read)]
       [message (format "Hello, ~a, nice to meet you!" name)])
  (display message))

