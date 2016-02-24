#lang racket/base

; Support function to supply a default value if an association list lookup fails.
(define (assoc-default v lst d)
  (let ([pair (assoc v lst)])
    (if pair (cadr pair) d)))

; Challenge: Write a version of the program that displays different
;            greetings for different people.

(define greetings
  '((Brian "Hello, ~a, nice to meet you!")
    (Duff  "~a, the race is on!")
    (Tom   "Yo ~a, what's happenin'")))

(define (pattern-for name)
  (assoc-default name greetings "Hello, ~a, this is a generic greeting"))

(display "What is your name? ")
(let ([name (read)])
  (printf (pattern-for name) name))


