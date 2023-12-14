#lang racket
(require "../advent.rkt")

(define (west  grid) (~> grid tilt))
(define (north grid) (~> grid transpose tilt transpose))
(define (east  grid) (~> grid (map reverse _) tilt (map reverse _)))
(define (south grid) (~> grid transpose (map reverse _) tilt (map reverse _) transpose))

(define (roll grid)
  (~> (group-consecutive grid (curry char=? #\#))
      (map (λ (l) (sort l char>?)) _)
      (apply append _)))

(define cycle     (compose1 east south west north))
(define grid      (map string->list (parse-aoc 14)))
(define tilt      (curry map roll))
(define transpose (curry apply map list))

(define (count-load grid)
  (for/sum ([ lst (in-list grid) ])
    (for/sum ([ rows (in-inclusive-range (length lst) 1 -1) ]
              [ c    (in-list lst)                          ])
      (if (char=? c #\O) rows 0))))

(define (floyd f val x0) ; Floyd "tortoise & hare" cycle detection algorithm
  (let*-values ([ (hare) (let loop ([ tortoise (f x0) ][ hare (f (f x0)) ])
                           (if (= (val tortoise) (val hare))
                               hare
                               (loop (f tortoise) (f (f hare))))) ]
                [ (tortoise mu) (let loop ([ mu 0 ][ tortoise x0 ][ hare hare ])
                                  (if (= (val tortoise) (val hare))
                                      (values tortoise mu)
                                      (loop (add1 mu) (f tortoise) (f hare)))) ])
    (let loop ([ hare (f tortoise) ][ lam 1 ])
      (if (= (val tortoise) (val hare))
          (values lam mu)
          (loop (f hare) (add1 lam))))))

(define part1 (compose1 count-load tilt transpose))

(define (part2 grid n)
  (let-values ([ (lam mu) (floyd cycle (λ (grid) (count-load (transpose grid))) grid) ])
    (count-load (transpose (iterate cycle grid (+ mu (modulo (- n mu) lam)))))))

;; Tests --------------------------------------------------------------------------------------

(check-equal? (part1 grid) 102497)
(check-equal? (part2 grid 1000000000) 105008)
