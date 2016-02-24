#lang racket

; Using a list of pairs (improper lists) to store quotes & authors
(define quotes '(("Toward yonder Sun"    . "Anonymous"     )
                 ("Winkin' and Blinkin'" . "Nod"           )
                 ("These aren't droids"  . "Obi-Wan Kenobi")))

(for ([q+a quotes])
  (match-define (cons quote author) q+a)
  (display (string-append author " says, \"" quote "\"\n")))
