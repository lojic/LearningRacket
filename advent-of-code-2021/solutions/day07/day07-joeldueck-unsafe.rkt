#lang racket

;; This version (part 2 only) inspired by Joel Dueck, and optimized
;; with unsafe arithmetic. The goal is efficiency.  After parsing the
;; input, it takes 17 micro-seconds to solve (2 micro-seconds less
;; than safe version :) )

(require "../../advent/advent.rkt" threading racket/unsafe/ops)

(define (solve positions)
  (let* ([ average (let loop ([ lst positions ][ sum 0 ][ count 0 ])
                     (if (null? lst)
                         (/ sum count)
                         (loop (cdr lst)
                               (unsafe-fx+ sum (car lst))
                               (unsafe-fx+ 1 count)))) ]
         [ pos (unsafe-fxmin (floor average)
                             (ceiling average)) ])
    (for/sum ([ n (in-list positions) ])
      (let ([ delta (unsafe-fx- n pos) ])
        (cond [ (unsafe-fx= delta 0) 0 ]
              [ (unsafe-fx> delta 0) (/ (unsafe-fx* delta (unsafe-fx+ 1 delta)) 2) ]
              [ else (let ([ delta (unsafe-fx- delta) ])
                       (/ (unsafe-fx* delta (unsafe-fx+ 1 delta)) 2)) ])))))

;; Tests --------------------------------------------------------------------------------------

(module+ test
  (require rackunit)
  (check-equal? (solve (csv-file->numbers "day07.txt")) 101079875))
