#lang racket

;; This version uses ideas I gleaned from other solutions.

(require "../advent/advent.rkt")

(define input (file->numbers "day01.txt"))

(define (count-increases lst) (count (λ (pair) (> (second pair) (first pair))) (zipn lst (cdr lst))))
(define (windows-3 lst)       (zipn lst (cdr lst) (cddr lst)))
(define (part1)               (count-increases input))
(define (part2)               (count-increases (map sum (windows-3 input))))
