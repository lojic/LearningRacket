#lang racket/base
(require racket/match)

; Challenge: Write a version of the program that displays different
;            greetings for different people.

(define (pattern name)
  (match name
    ['Brian "Hello, ~a, nice to meet you!"]
    ['Duff  "~a, the race is on!"]
    ['Tom   "Yo ~a, what's happenin'"]
    [_      "Hello, ~a, this is a generic greeting"]))

(display "What is your name? ")
(define name (read))
(printf (pattern name) name)


