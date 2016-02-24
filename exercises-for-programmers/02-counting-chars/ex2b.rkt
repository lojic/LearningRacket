#lang racket/base

; Challenge: If the user enters nothing, state that they must enter something into the program.

(display "What is the input string?")

(let loop ([str (read-line)])
  (define len (string-length str))
  (cond [(> len 0) (printf "~s has ~a characters." str len)]
        [else      (display "Please enter something: ")
                   (loop (read-line))]))
