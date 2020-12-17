#lang racket
(require rackunit)

(define cycles 6)
(define (key x y [z 0] [w 0]) (list x y z w))

(define (parse fname)
  (define (is-active? lines x y) (char=? #\# (string-ref (list-ref lines y) x)))
  (let* ([ lines (file->lines fname)         ]
         [ x-dim (string-length (car lines)) ]
         [ y-dim (length lines)              ]
         [ delta (* 2 cycles)                ]
         [ hsh (for*/hash ([ x (in-range x-dim) ][ y (in-range y-dim) ]
                           #:when (is-active? lines x y))
                 (values (key x y) #t)) ])
    (list hsh (+ x-dim delta) (+ y-dim delta) (+ 1 delta) 0)))

(define (set-active? hsh key active-neighbors)
  (let ([ n (apply active-neighbors hsh key) ])
    (or (= n 3) (and (= n 2) (hash-ref hsh key #f)))))

(define (triple n) (list (- n 1) n (+ n 1)))

(define (part1 hsh x-dim y-dim z-dim w-dim)
  (define (active-neighbors hsh x0 y0 z0 _)
    (for*/sum ([ x (triple x0) ][ y (triple y0) ][ z (triple z0) ]
               #:when (and (not (and (= x0 x) (= y0 y) (= z0 z)))
                           (hash-ref hsh (key x y z) #f))) 1))

  (λ (hsh)
    (for*/hash ([ x (in-range (- cycles) (+ x-dim cycles 1)) ]
                [ y (in-range (- cycles) (+ y-dim cycles 1)) ]
                [ z (in-range (- cycles) (+ cycles 1))       ]
                #:when (set-active? hsh (key x y z) active-neighbors))
      (values (key x y z) #t))))

(define (part2 hsh x-dim y-dim z-dim w-dim)
  (define (active-neighbors hsh x0 y0 z0 w0)
    (for*/sum ([ x (triple x0) ] [ y (triple y0) ] [ z (triple z0) ] [ w (triple w0) ]
               #:when (and (not (and (= x0 x) (= y0 y) (= z0 z) (= w0 w)))
                           (hash-ref hsh (key x y z w) #f))) 1))

  (λ (hsh)
    (for*/hash ([ x (in-range (- cycles) (+ x-dim cycles 1)) ]
                [ y (in-range (- cycles) (+ y-dim cycles 1)) ]
                [ z (in-range (- cycles) (+ cycles 1))       ]
                [ w (in-range (- cycles) (+ cycles 1))       ]
                #:when (set-active? hsh (key x y z w) active-neighbors))
      (values (key x y z w) #t))))

(define (run part)
  (let* ([ lst (parse "day17.txt") ][ cycle (apply part lst) ])
    (hash-count (cycle (cycle (cycle (cycle (cycle (cycle (car lst))))))))))

(check-equal? (run part1) 232)
(check-equal? (run part2) 1620)
