#lang racket

(require "etl.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define suite
    (test-suite
     "Tests for the etl exercise"

     (test-case "transform one value"
                (check-equal? (transform #hash((1 . (#\W)))) #hash((#\w . 1))))
     (test-case "more values"
                (check-equal? (transform #hash((1 . (#\W #\x)) (2 . (#\a #\B))))
                              #hash((#\a . 2) (#\b . 2) (#\w . 1) (#\x . 1))))
     (test-case "full database"
                (check-equal?
                 (transform #hash((1 . (#\A #\E #\I #\O #\U #\L #\N #\R #\S #\T))
                                  (2 . (#\D #\G))
                                  (3 . (#\B #\C #\M #\P))
                                  (4 . (#\F #\H #\V #\W #\Y))
                                  (5 . (#\K))
                                  (8 . (#\J #\X))
                                  (10 . (#\Q #\Z))))
                 #hash((#\a . 1) (#\b . 3) (#\c . 3) (#\d . 2) (#\e . 1) (#\f . 4) (#\g . 2)
                       (#\h . 4) (#\i . 1) (#\j . 8) (#\k . 5) (#\l . 1) (#\m . 3) (#\n . 1)
                       (#\o . 1) (#\p . 3) (#\q . 10) (#\r . 1) (#\s . 1) (#\t . 1) (#\u . 1)
                       (#\v . 4) (#\w . 4) (#\x . 8) (#\y . 4) (#\z . 10)
                       )))
     ))
     

  (run-tests suite))
