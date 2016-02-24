#lang racket/base

(display "What is the input string?")
(define str (read-line))
(printf "~s has ~a characters." str (string-length str))
