#lang racket
(require "../lojic.rkt")

(let ([quote  (gets "What is the quote? ")]
      [author (gets "Who said it? "      )])
  (display (string-append author " says, \"" quote "\"")))

