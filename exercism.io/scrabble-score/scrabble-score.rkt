#lang racket
(require "../etl/etl.rkt")
(require threading)
(provide score)

(define points (transform #hash((1 . (#\A #\E #\I #\O #\U #\L #\N #\R #\S #\T))
                                (2 . (#\D #\G))
                                (3 . (#\B #\C #\M #\P))
                                (4 . (#\F #\H #\V #\W #\Y))
                                (5 . (#\K))
                                (8 . (#\J #\X))
                                (10 . (#\Q #\Z)))))

(define (score word)
  (apply + (~> word
               string-trim
               string-downcase
               string->list
               (map (curry hash-ref points) _))))
