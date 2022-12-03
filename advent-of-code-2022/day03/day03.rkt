#lang racket

(require "../advent.rkt" threading)

(define in (parse-aoc 3 string->list))

(define (priority c)
  (let ([ n (char->integer c) ])
    (if (> n 96)
        (- n 96)    ; Lowercase letter
        (- n 38)))) ; Uppercase letter

(define (solve transform in)
  (for/sum ([ group (transform in) ])
    (~> (apply set-intersect group)
        set-first
        priority)))

(solve (curry map split-2) in) ; Part 1

(solve (curry chunk 3) in)     ; Part 2
