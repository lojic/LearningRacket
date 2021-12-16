#lang racket ; This version uses ideas gleaned from others.

(require data/heap threading)
(struct path (risk x y))

(define (solve)
  (let* ([ dim*    100        ]
         [ dim     (* 5 dim*) ]
         [ vec     (~>> (file->lines "day15.txt")
                        (apply string-append)
                        string->list
                        (map (λ (c) (- (char->integer c) 48)))
                        list->vector) ]
         [ visited (make-vector (* dim dim) #f) ]
         [ paths   (make-heap (λ ( p1 p2) (< (path-risk p1) (path-risk p2)))) ])

    (define (visited? x y) (vector-ref visited (+ (* y dim) x)))
    (define (wrap n)       (if (> n 9) (- n 9) n))

    (define (get x y)
      (let-values ([ (tile-x x) (quotient/remainder x dim*) ]
                   [ (tile-y y) (quotient/remainder y dim*) ])
        (let* ([ risk (vector-ref vec (+ (* y dim*) x)) ])
          (wrap (+ risk tile-x tile-y)))))

    (heap-add! paths (path 0 0 0))

    (let loop ()
      (match-let ([ (path risk x y) (heap-min paths) ])
        (heap-remove-min! paths)
        (cond [ (visited? x y)                           (loop) ]
              [ (and (= x (sub1 dim)) (= y (sub1 dim)))  risk   ]
              [ else (vector-set! visited (+ (* y dim) x) #t)
                     (for ([ nx (list (- x 1) (+ x 1) x x) ]
                           [ ny (list y y (- y 1) (+ y 1)) ]
                           #:when (and (<= 0 nx (sub1 dim))
                                       (<= 0 ny (sub1 dim))
                                       (not (visited? nx ny))))
                       (heap-add! paths (path (+ risk (get nx ny)) nx ny)))
                     (loop) ])))))
(time (solve))
