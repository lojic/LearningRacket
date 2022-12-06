#lang racket
(require "../advent.rkt")

(define in (car (parse-aoc 6 string->list)))

(define (unique-group-end n lst)
  (~> (windows n lst)
      (enumerate _ n)
      (findf (compose not check-duplicates car) _)
      cdr))

(unique-group-end 4 in)  ; Part 1
(unique-group-end 14 in) ; Part 2
