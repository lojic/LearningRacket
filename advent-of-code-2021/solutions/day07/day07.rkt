#lang racket

(require "../../advent/advent.rkt" threading)

(define (solve cost positions)
  (~> (for/list ([ pos (range (list-max positions)) ])
        (for/sum ([ n positions ])
          (cost (abs (- n pos)))))
      list-min))

(define part1-cost identity)

(define (part2-cost n)
  (/ (* n (add1 n)) 2))

;; Tests --------------------------------------------------------------------------------------

(module+ test
  (require rackunit)
  (check-equal? (solve part1-cost (csv-file->numbers "day07.txt")) 351901)
  (check-equal? (solve part2-cost (csv-file->numbers "day07.txt")) 101079875))
