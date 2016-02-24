#lang racket/base
(require racket/match)

; Challenge: Write a version of the program that displays different
;            greetings for different people.

(define greetings (hash 'Brian "Hello, ~a, nice to meet you!"
                        'Duff  "~a, the race is on!"
                        'Tom   "Yo ~a, what's happenin'"))

(display "What is your name? ")
(define name (read))
(printf
 (hash-ref greetings name "Hello, ~a, this is a generic greeting")
 name)


