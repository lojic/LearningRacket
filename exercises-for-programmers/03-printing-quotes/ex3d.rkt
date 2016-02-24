#lang racket

; Using list of hashtables to store quotes & authors
(define quotes '(#hash((text . "Toward yonder Sun")    (author . "Anonymous"     ))
                 #hash((text . "Winkin' and Blinkin'") (author . "Nod"           ))
                 #hash((text . "These aren't droids")  (author . "Obi-Wan Kenobi"))))

(for ([hsh quotes])
  (display (string-append (hash-ref hsh 'author) " says, \"" (hash-ref hsh 'text) "\"\n")))
