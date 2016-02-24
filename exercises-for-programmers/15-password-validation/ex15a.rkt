#lang racket
(require "../lojic.rkt")

(display (if (string=? "sesame" (gets "What is the password? "))
             "Welcome!"
             "I don't know you."))
