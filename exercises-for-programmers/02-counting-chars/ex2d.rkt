#lang racket/base

; Copying Duff's module example c.exs
(module counting-chars racket/base
  (require racket/match)
  (provide go)

  ; support function
  (define (pipe . fs)
    (apply compose (reverse fs)))

  (define (retrieve-input)
    (display "What is the input string? ") (read-line))

  (define (generate-output str)
    (match str
      ["" (printf "You need to enter some input!\n")
          (go)]
      [_  (printf "~s has ~a characters." str (string-length str))]))

  (define (go)
    ((pipe retrieve-input
           generate-output))))

(require 'counting-chars)
(go)