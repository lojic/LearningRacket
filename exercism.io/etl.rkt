#lang racket
(provide transform)

(define (transform input)
  (for*/hash ([ (score letters) input   ]
              [ letter          letters ])
             (values (char-downcase letter) score)))
