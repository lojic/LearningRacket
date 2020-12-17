#lang racket
(require rackunit)

(define (key x y [z 0] [w 0]) (list x y z w))

(define (parse fname cycles)
  (define (is-active? lines x y) (char=? #\# (string-ref (list-ref lines y) x)))
  (let* ([ lines (file->lines fname)         ]
         [ x-dim (string-length (car lines)) ]
         [ y-dim (length lines)              ]
         [ delta (* 2 cycles)                ]
         [ hsh (for*/hash ([ x (in-range x-dim) ][ y (in-range y-dim) ]
                           #:when (is-active? lines x y))
                 (values (key x y) #t)) ])
    (list hsh (+ x-dim delta) (+ y-dim delta) (+ 1 delta) 0)))

(define (run 4d? cycles)
  (match-let ([ (list hsh x-dim y-dim z-dim w-dim) (parse "day17.txt" cycles) ])
    (define (triple n) (list (- n 1) n (+ n 1)))
    (define (iterate fun arg n) (if (zero? n) arg (iterate fun (fun arg) (sub1 n))))
    (define (set-active? hsh key active-neighbors)
      (let ([ n (apply active-neighbors hsh key) ])
        (or (= n 3) (and (= n 2) (hash-ref hsh key #f)))))
    (define (active-neighbors hsh x0 y0 z0 w0)
      (for*/sum ([ x (triple x0) ] [ y (triple y0) ] [ z (triple z0) ][ w (if 4d? (triple w0) (in-range 1)) ]
                 #:when (and (not (and (= x0 x) (= y0 y) (= z0 z) (= w0 w)))
                             (hash-ref hsh (key x y z w) #f))) 1))
    (define (cycle hsh)
      (for*/hash ([ x (in-range (- cycles) (+ x-dim cycles 1)) ]
                  [ y (in-range (- cycles) (+ y-dim cycles 1)) ]
                  [ z (in-range (- cycles) (+ cycles 1))       ]
                  [ w (if 4d? (in-range (- cycles) (+ cycles 1)) (in-range 1)) ]
                  #:when (set-active? hsh (key x y z w) active-neighbors))
        (values (key x y z w) #t)))
  (hash-count (iterate cycle hsh 6))))

(check-not-false (and (= (run #f 6) 232) (= (run #t 6) 1620)))
