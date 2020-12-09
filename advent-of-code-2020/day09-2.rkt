#lang racket

(define (run n fname)
  (let ([ r (let find-range ([lst (map string->number (file->lines fname))])
              (or (let loop ([lst lst][sum 0][result '()])
                    (cond [ (= sum n) result ]
                          [ (or (> sum n) (null? lst)) #f ]
                          [ else (let ([ x (car lst) ])
                                   (loop (cdr lst) (+ sum x) (cons x result))) ]))
                  (find-range (cdr lst)))) ])
    (+ (apply min r) (apply max r))))


(module+ test
  (require rackunit)
  (check-equal? (run 127 "day09-test.txt") 62)
  (check-equal? (run 20874512 "day09.txt") 3012420))
