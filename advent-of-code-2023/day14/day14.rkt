#lang racket
(require "../advent.rkt")

(define (north grid) (~> grid transpose roll transpose))
(define (west grid)  (roll grid))
(define (east grid)  (~> grid (map reverse _) roll (map reverse _)))
(define (south grid) (~> grid transpose (map reverse _) roll (map reverse _) transpose))

(define cycle     (compose1 east south west north))
(define grid      (map string->list (parse-aoc 14)))
(define transpose (curry apply map list))

(define (roll grid)
  (define (split-rocks lst)
    (define (cube? c)  (char=? #\# c))
    (define (rolls? c) (not (cube? c)))
    
    (define (helper lst fun)
      (if (null? lst)
          '()
          (let-values ([ (h t) (splitf-at lst fun) ])
            (cons h (helper t (if (eq? fun cube?) rolls? cube?))))))
    
    (helper lst (if (cube? (car lst)) cube? rolls?)))
  
  (map (λ (lst)
         (~> (split-rocks lst)
             (map (λ (l) (sort l char>?)) _)
             (apply append _)))
       grid))

(define (count-load grid)
  (for/sum ([ lst (in-list grid) ])
    (for/sum ([ c    (in-list lst)                          ]
              [ rows (in-inclusive-range (length lst) 1 -1) ])
      (if (char=? c #\O) rows 0))))

(define (part1)
  (count-load (roll (transpose grid))))

(define (part2)
  (count-load (transpose (iterate cycle grid 168))))

;; Tests --------------------------------------------------------------------------------------

(check-equal? (part1) 102497)
(check-equal? (part2) 105008)


