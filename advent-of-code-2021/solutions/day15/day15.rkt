#lang racket

(require graph threading)
(define directions (list 0-i 1+0i 0+i -1+0i))

(define (solve part file-name)
  (let*-values ([ (dim g)   (parse part file-name) ]
                [ (paths _) (dijkstra g 0)         ]
                [ (vertex)  (make-rectangular (sub1 dim)
                                              (sub1 dim)) ])
    (hash-ref paths vertex)))

(define part1 identity)

(define (part2 vec*)
  (define (set-tile! vec dim* dim tx ty)
    (define (vset! v x y val)
      (let* ([ val (+ val tx ty)                ]
             [ val (if (> val 9) (- val 9) val) ]
             [ x*  (+ (* tx dim*) x)            ]
             [ y*  (+ (* ty dim*) y)            ])
        (vector-set! v (idx dim x* y*) val)))
    
    (for* ([ x (range dim*) ]
           [ y (range dim*) ])
      (vset! vec x y (vector-ref vec* (idx dim* x y)))))
  
  (let* ([ dim* (sqrt (vector-length vec*)) ]
         [ dim  (* 5 dim*)                  ]
         [ vec  (make-vector (* dim dim))   ])
    (for* ([ tx (range 5) ]
           [ ty (range 5) ])
      (set-tile! vec dim* dim tx ty))

    vec))

(define (create-graph dim vec)
  (define (neighbor-indices dim i)
    (define (valid-index? dim i)
      (and (<= 0 (real-part i) (sub1 dim))
           (<= 0 (imag-part i) (sub1 dim))))
    
    (~>> directions
         (map (curry + i))
         (filter (curry valid-index? dim))))

  (~> (for*/list ([ x (range dim) ]
                  [ y (range dim) ])
        (let ([ src (make-rectangular x y) ])
          (for/list ([ dst (neighbor-indices dim src) ])
            (let ([ dst-x (real-part dst) ]
                  [ dst-y (imag-part dst) ])
              (list (vector-ref vec (idx dim dst-x dst-y)) ; Weight
                    src                                    ; Source
                    dst)))))                               ; Destination
      append*
      weighted-graph/directed))

(define (parse part file-name)
  (define (parse-file file-name)
    (~>> (file->lines file-name)
         (apply string-append _)
         string->list
         (map (λ (c) (- (char->integer c) 48)))
         list->vector))
  
  (let* ([ vec (part (parse-file file-name)) ]
         [ dim (sqrt (vector-length vec))    ])
    (values dim (create-graph dim vec))))

(define (idx dim x y) (+ (* dim y) x))

;; Tests --------------------------------------------------------------------------------------

(module+ test
  (require rackunit)
  (check-equal? (solve part1 "day15.txt") 687)
  (check-equal? (solve part2 "day15.txt") 2957))

