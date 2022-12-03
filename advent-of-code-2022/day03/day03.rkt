#lang racket
(require "../advent.rkt" threading)

(define in (parse-aoc 3 string->list)) ; Parse each line into a list of chars

(define priority
  (compose add1
           (curry string-index-of "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")))

(define (solve transform in)
  (for/sum ([ group (transform in) ])
    (~> (apply set-intersect group)
        set-first
        priority)))

(solve (curry map split-2) in) ; Part 1

(solve (curry chunk 3) in)     ; Part 2
