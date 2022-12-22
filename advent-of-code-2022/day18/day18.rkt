#lang racket
(require "../advent.rkt")

(define in (parse-aoc 18 numbers))

(define-values (xmin xmax ymin ymax zmin zmax)
  (let* ([ xs (map car   in) ][ xmin (sub1 (list-min xs)) ][ xmax (add1 (list-max xs)) ]
         [ ys (map cadr  in) ][ ymin (sub1 (list-min ys)) ][ ymax (add1 (list-max ys)) ]
         [ zs (map caddr in) ][ zmin (sub1 (list-min zs)) ][ zmax (add1 (list-max zs)) ])
    (values xmin xmax ymin ymax zmin zmax)))

(define (part1 in)
  (for/fold ([ sides (hash) ])
            ([ cube (in-list in) ])
    (for/fold ([ sides sides ])
              ([ side (in-list (cube-sides cube)) ])
      (if (hash-ref sides side #f)
          (hash-remove sides side)
          (hash-set sides side #t)))))

(define (part2 in)
  (expand-steam (list xmin ymin zmin)
                (part1 in)
                (make-immutable-hash (map (λ (cube)
                                            (cons cube #t))
                                          in))))

(define (expand-steam cube sides seen [ sum 0 ])
  (if (seen? cube seen)
      (values sum seen)
      (let loop ([ moves (next-cubes cube seen)     ]
                 [ sum   (+ sum (faces cube sides)) ]
                 [ seen  (mark-seen cube seen)      ])
        (if (null? moves)
            (values sum seen)
            (let ([ move (car moves) ])
              (let-values ([ (sum* seen*) (expand-steam move sides seen sum) ])
                (loop (cdr moves)
                      sum*
                      seen*)))))))

(define (seen? cube seen)     (hash-ref seen cube #f))
(define (mark-seen cube seen) (hash-set seen cube #t))

(define (cube-sides cube)
  (match-let ([ (list x y z) cube ])
    (list (list       x       y       z  'z)
          (list       x       y (sub1 z) 'z)
          (list       x       y       z  'x)
          (list (sub1 x)      y       z  'x)
          (list       x       y       z  'y)
          (list       x (sub1 y)      z  'y))))

(define (next-cubes cube seen)
  (define (valid? c)
    (match-let ([ (list x y z) c ])
      (not (or (< x xmin) (> x xmax)
               (< y ymin) (> y ymax)
               (< z zmin) (> z zmax)
               (hash-ref seen c #f)))))

  (match-let ([ (list x y z) cube ])
    (filter valid? (list (list (add1 x) y z)
                         (list (sub1 x) y z)
                         (list x (add1 y) z)
                         (list x (sub1 y) z)
                         (list x y (add1 z))
                         (list x y (sub1 z))))))

(define (faces cube sides)
  (for/sum ([ side (cube-sides cube) ])
    (if (hash-ref sides side #f) 1 0)))

(time (check-equal? (hash-count (part1 in)) 3390))
(time (check-equal? (let-values ([(sum _) (part2 in)]) sum) 2058))
