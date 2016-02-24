#lang racket
(require "../lojic.rkt")

; Factor out retrieve per Duff's solution. This solution changes the order of retrieval of the
; parts of speech, but it's concise.
(define (retrieve part-of-speech)
  (gets (format "Enter a ~a: " part-of-speech)))

(apply printf "Do you ~a your ~a ~a ~a? That's hilarious!"
              (map retrieve '(verb adjective noun adverb)))
