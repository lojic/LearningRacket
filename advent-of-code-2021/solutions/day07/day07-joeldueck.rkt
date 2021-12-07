#lang racket

;; This version (part 2 only) inspired by Joel Dueck. The goal is
;; efficiency.  After parsing the input, it takes 19 micro-seconds to
;; solve.

(require "../../advent/advent.rkt" threading)

(define (solve positions)
  (let* ([ average (let loop ([ lst positions ][ sum 0 ][ count 0 ])
                     (if (null? lst)
                         (/ sum count)
                         (loop (cdr lst) (+ sum (car lst)) (add1 count)))) ]
         [ pos (min (floor average) (ceiling average)) ])
    (for/sum ([ n (in-list positions) ])
      (let ([ delta (- n pos) ])
        (cond [ (= delta 0) 0 ]
              [ (> delta 0) (/ (* delta (add1 delta)) 2) ]
              [ else (let ([ delta (- delta) ])
                       (/ (* delta (add1 delta)) 2)) ])))))

;; Tests --------------------------------------------------------------------------------------

(module+ test
  (require rackunit)
  (check-equal? (solve (csv-file->numbers "day07.txt")) 101079875))
