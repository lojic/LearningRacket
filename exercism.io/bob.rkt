#lang racket

(provide hey)

(define (hey input)
  (cond [ (nothing?  input) "Fine. Be that way!" ]
        [ (question? input) "Sure."              ]
        [ (yelling?  input) "Whoa, chill out!"   ]
        [ else              "Whatever."          ]))

(define (nothing? input)
  (not (non-empty-string? (string-trim input))))
  
(define (yelling? input)
  (all-caps? input))

(define (question? input)
  (string-suffix? input "?"))

(define (all-caps? input)
  (and (string=? (string-upcase input) input)
       (regexp-match? #px"\\p{Lu}" input)))
       
       
