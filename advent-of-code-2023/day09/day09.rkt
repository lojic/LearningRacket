#lang racket
(require "../advent.rkt")

(define lines (parse-aoc 9 numbers))

(define (compute-deltas lst)
  (cond [ (null? (cdr lst)) '() ]
        [ else (cons (- (first lst)
                        (second lst))
                     (compute-deltas (cdr lst))) ]))

(define (extrapolate lst)
  (let loop ([ lst (reverse lst) ][ results '() ])
    (if (andmap zero? lst)
        (list-sum (map car results))
        (loop (compute-deltas lst) (cons lst results)))))

(define part1 (compose1 list-sum (curry map extrapolate)))
(define part2 (compose1 part1 (curry map reverse)))

;; Tests --------------------------------------------------------------------------------------

(check-equal? (part1 lines) 1681758908)
(check-equal? (part2 lines) 803)
