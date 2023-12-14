#lang racket
(require "../advent.rkt")

(define (west  grid) (~> grid tilt))
(define (north grid) (~> grid transpose tilt transpose))
(define (east  grid) (~> grid (map reverse _) tilt (map reverse _)))
(define (south grid) (~> grid transpose (map reverse _) tilt (map reverse _) transpose))

(define ((class t) c)
  (if (char=? #\# t)
      (char=? #\# c)
      (not (char=? #\# c))))

(define (count-load grid)
  (for/sum ([ lst (in-list grid) ])
    (for/sum ([ rows (in-inclusive-range (length lst) 1 -1) ]
              [ c    (in-list lst)                          ])
      (if (char=? c #\O) rows 0))))

(define (split-rocks lst)
  (if (null? lst)
      '()
      (let-values ([ (h t) (splitf-at lst (class (car lst))) ])
        (cons h (split-rocks t)))))

(define (roll grid)
  (~> (split-rocks grid)
      (map (λ (l) (sort l char>?)) _)
      (apply append _)))

(define cycle     (compose1 east south west north))
(define grid      (map string->list (parse-aoc 14)))
(define tilt      (curry map roll))
(define transpose (curry apply map list))
(define part1     (compose1 count-load tilt transpose))

(define (part2)
  (count-load (transpose (iterate cycle grid 168))))

;; Tests --------------------------------------------------------------------------------------

(check-equal? (part1 grid) 102497)
(check-equal? (part2) 105008)
