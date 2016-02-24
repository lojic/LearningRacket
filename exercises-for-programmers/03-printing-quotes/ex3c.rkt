#lang racket

; Using structs to store quotes & authors
(struct quote (text author))
(define quotes '((quote "Toward yonder Sun"    "Anonymous"     )
                 (quote "Winkin' and Blinkin'" "Nod"           )
                 (quote "These aren't droids"  "Obi-Wan Kenobi")))

(for ([q+a quotes])
  (match-define (list quote author) q+a)
  (display (string-append author " says, \"" quote "\"\n")))
