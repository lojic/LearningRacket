#lang racket

(require "../advent.rkt")

;; Parse input into the form:
;; ( (<id> (<n> <color>) (<n> <color) ... ) ... )
(define in (map (parallel-combine cons cadr (compose1 (curry chunk 2) (curry (flip drop) 2)))
                (parse-aoc 2 atoms)))

(define (max-pixel lst [red 0] [green 0] [blue 0])
  (if (null? lst)
      (list red green blue)
      (match (car lst)
        [ (list n "red")   (max-pixel (cdr lst) (max n red) green blue) ]
        [ (list n "green") (max-pixel (cdr lst) red (max n green) blue) ]
        [ (list n "blue")  (max-pixel (cdr lst) red green (max n blue)) ])))

;; Parts --------------------------------------------------------------------------------------

(define (part1)
  (define max-rgb '(12 13 14))
  (~> (map (parallel-combine cons car (compose1 max-pixel cdr)) in)
      (filter (compose1 (curry andmap >= max-rgb) cdr) _)
      (map car _)
      (list-sum _)))

(define (part2)
  (~> (map (compose1 max-pixel cdr) in)
      (map list-prod _)
      (list-sum)))

;; Tests --------------------------------------------------------------------------------------

(check-equal? (part1) 2528)
(check-equal? (part2) 67363)
