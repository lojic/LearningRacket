#lang racket

(require "../../advent/advent.rkt")

(define (sum-fuel cost positions pos)
  (for/sum ([ n positions ])
    (cost (abs (- n pos)))))

(define (solve cost positions)
  (for/fold ([ least 1000000000            ])
            ([ pos   (range (argmin identity positions)
                            (argmax identity positions)) ])
    (min least (sum-fuel cost positions pos))))

(define part1-cost identity)

(define (part2-cost n)
  (/ (* n (add1 n)) 2))

;; Tests --------------------------------------------------------------------------------------

(module+ test
  (require rackunit)
  (check-equal? (solve part1-cost (csv-file->numbers "day07.txt")) 351901)
  (check-equal? (solve part2-cost (csv-file->numbers "day07.txt")) 101079875))
