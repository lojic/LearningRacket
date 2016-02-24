#lang racket

; Using a list of 2-tuples to store quotes & authors
(define quotes '(("Toward yonder Sun"    "Anonymous"     )
                 ("Winkin' and Blinkin'" "Nod"           )
                 ("These aren't droids"  "Obi-Wan Kenobi")))

(for ([q+a quotes])
  (match-define (list quote author) q+a)
  (display (string-append author " says, \"" quote "\"\n")))
