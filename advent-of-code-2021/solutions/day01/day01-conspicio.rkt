#lang racket

;; This version uses ideas I gleaned from other solutions.

(require "../../advent/advent.rkt")

(define example '(199 200 208 210 200 207 240 269 260 263))
(define input (file->list "day01.txt"))

;; Return a count of the number of times a number in the list is
;; greater than the preceding number.
(define (count-increases lst)
  (count (λ (pair) (> (second pair) (first pair))) (zipn lst (cdr lst))))

;; Return a list of 3-element sliding windows from a list.
(define (windows-3 lst) (zipn lst (cdr lst) (cddr lst)))

(define (part1) (count-increases input))

(define (part2) (count-increases (map sum (windows-3 input))))


(module+ test
  (require rackunit)

  ;; Part 1
  (check-equal? (part1) 1616)

  ;; Part 2
  (check-equal? (part2) 1645)

  ;; count-increases
  (check-equal? (count-increases example) 7)

  ;; windows-3
  (check-equal? (windows-3 '(1 2 3 4 5 6))
                '((1 2 3) (2 3 4) (3 4 5) (4 5 6)))

  )
