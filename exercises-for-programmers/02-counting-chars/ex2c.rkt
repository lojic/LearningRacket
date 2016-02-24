#lang racket/base

; Challenge: If the user enters nothing, state that they must enter something into the program.

; Use if instead of cond

(display "What is the input string?")

(let loop ([str (read-line)])
  (define len (string-length str))
  (if (> len 0)
      (printf "~s has ~a characters." str len)
      (begin
        (display "Please enter something: ")
        (loop (read-line)))))
