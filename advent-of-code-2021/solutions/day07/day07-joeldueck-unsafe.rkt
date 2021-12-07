#lang racket

;; This version (part 2 only) inspired by Joel Dueck, and optimized
;; with unsafe arithmetic. The goal is efficiency.  After parsing the
;; input, it takes 17 micro-seconds to solve (2 micro-seconds less
;; than safe version :) )

(require "../../advent/advent.rkt" threading racket/unsafe/ops)

(define i* unsafe-fx*)
(define i+ unsafe-fx+)
(define i- unsafe-fx-)
(define i= unsafe-fx=)
(define i> unsafe-fx>)
(define imin unsafe-fxmin)

(define (solve positions)
  (let* ([ average (let loop ([ lst positions ][ sum 0 ][ count 0 ])
                     (if (null? lst)
                         (/ sum count)
                         (loop (cdr lst) (i+ sum (car lst)) (i+ 1 count)))) ]
         [ pos (imin (floor average) (ceiling average)) ])
    (for/sum ([ n (in-list positions) ])
      (let ([ delta (i- n pos) ])
        (cond [ (i= delta 0) 0 ]
              [ (i> delta 0) (/ (i* delta (i+ 1 delta)) 2) ]
              [ else (let ([ delta (i- delta) ])
                       (/ (i* delta (i+ 1 delta)) 2)) ])))))

;; Tests --------------------------------------------------------------------------------------

(module+ test
  (require rackunit)
  (check-equal? (solve (csv-file->numbers "day07.txt")) 101079875))
