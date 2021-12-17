#lang racket

(require threading "../../advent/advent.rkt")

(define-values (x-min x-max y-min y-max) (values 206 250 -105 -57))

(define (solve part)
  (~>> (for*/list ([ dx (inclusive-range 1            (add1 x-max))    ]
                   [ dy (inclusive-range (sub1 y-min) (* 2 (- y-min))) ])
         (shoot dx dy))
       (filter (compose not null?))
       part))

(define part1 (λ (l) (~>> l append* (map third) list-max)))
(define part2 length)

(define (shoot dx dy [x 0] [y 0] [top 0] [results '()])
  (if (or (> x x-max) (< y y-min))
      results
      (let ([ results (if (or (< x x-min) (> y y-max))
                          results
                          (cons (list dx dy top) results)) ]
            [ dx (cond [ (> dx 0) (sub1 dx) ]
                       [ (< dx 0) (add1 dx) ]
                       [ else     dx        ]) ]
            [ dy  (sub1 dy)   ]
            [ x   (+ x dx)    ]
            [ y   (+ y dy)    ]
            [ top (max y top) ] )
        (shoot dx dy x y top results))))

;; Tests --------------------------------------------------------------------------------------

(module+ test
  (require rackunit)
  (check-equal? (solve part1) 5460)
  (check-equal? (solve part2) 3618))
