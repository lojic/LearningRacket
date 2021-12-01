#lang racket

(require "../../advent/advent.rkt")

(define input (file->numbers "day01.txt"))

(define (count-increases lst)
  (car (foldl (match-lambda* [ (list n (cons count last)) (cons (+ count (if (> n last) 1 0)) n) ])
              (cons 0 (car lst))
              (cdr lst))))

(define (windows-3 lst) (zipn lst (cdr lst) (cddr lst)))

(define (part1) (count-increases input))
(define (part2) (count-increases (map sum (windows-3 input))))
