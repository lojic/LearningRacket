#lang racket
(provide convert)
(define drops #hash((3 . "Pling") (5 . "Plang") (7 . "Plong")))

(define (convert number)
  (join number (sounds number)))

(define (sounds number)
  (for/list ([(value sound) drops] #:when (= 0 (remainder number value)))
            sound))

(define (join number lst)
  (if (null? lst)
      (number->string number)
      (string-join lst)))