#lang racket

(require "./advent.rkt")
(define grid (grid->hash (parse-aoc 10 digits)))

(define (solve score)
  (for/sum ([ (head level) (in-hash grid) ] #:when (= level 0))
    (score (dfs head -1 '()))))

(define (dfs pos prev result)
  (let ([ level (hash-ref grid pos -1) ])
    (cond [ (not (= level (add1 prev)))  result ]
          [ (= level 9)                  (cons pos result) ]
          [ else                         (for/fold ([ result result ])
                                                   ([ dir (in-list '(1 +i -1 -i)) ])
                                           (dfs (+ pos dir) level result)) ])))

;; --------------------------------------------------------------------------------------------

(check-equal? (solve (compose set-count list->set)) 644)
(check-equal? (solve length) 1366)
