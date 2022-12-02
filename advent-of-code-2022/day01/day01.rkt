#lang racket

(require "../advent.rkt" threading)

(define in (parse-aoc 1 numbers #:sep "\n\n"))

;; Part 1 -------------------------------------------------------------------------------------

(~> (map list-sum in)
    list-max)

;; Part 2 -------------------------------------------------------------------------------------

(~> (map list-sum in)
    (sort _ >)
    (take _ 3)
    (list-sum _))
