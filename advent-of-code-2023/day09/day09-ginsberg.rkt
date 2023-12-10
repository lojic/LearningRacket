#lang racket
(require "../advent.rkt")

;; Port of Todd Ginsberg's solution

(define lines (parse-aoc 9 numbers))

(define (extrapolate lst)
  (if (andmap zero? lst)
      0
      (let ([ deltas (map (curry apply -) (windows 2 lst)) ])
        (+ (car lst) (extrapolate deltas)))))

(define part2 (compose1 list-sum (curry map extrapolate)))
(define part1 (compose1 part2 (curry map reverse)))

;; Tests --------------------------------------------------------------------------------------

(check-equal? (part1 lines) 1681758908)
(check-equal? (part2 lines) 803)
