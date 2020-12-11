#lang racket

(require "./day11-1.rkt")

(define (get-seat obj row col dr dc)
  (let ([ r (+ row dr) ] [ c (+ col dc) ])
    (if (valid-row-col? obj r c)
        (let ([ seat (vget obj r c) ])
          (if (or (is-empty? seat) (is-occupied? seat))
              seat
              (get-seat obj r c dr dc)))
        floor-loc)))

(module+ test
  (require rackunit)
  (check-equal? (run "day11-test.txt" get-seat 5) 26)
  (check-equal? (time (run "day11.txt" get-seat 5)) 2149))