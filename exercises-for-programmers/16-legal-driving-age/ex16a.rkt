#lang racket
(require "../lojic.rkt")

(displayln (if (< (get-num "What is your age?") 16)
               "You are not old enough to legally drive."
               "You are old enough to legally drive."))
