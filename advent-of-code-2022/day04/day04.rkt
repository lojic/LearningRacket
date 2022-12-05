#lang racket
(require "../advent.rkt")

(define in (parse-aoc 4 numbers))

(define (part1? a b) (or (subset? a b) (subset? b a)))

(define (part2? a b) (not (set-empty? (set-intersect a b))))

(define ((need-reorg? part?) lst)
  (match-let ([(list a b c d) lst])
    (part? (inclusive-range a b) (inclusive-range c d))))

(count (need-reorg? part1?) in) ; Part 1

(count (need-reorg? part2?) in) ; Part 2
