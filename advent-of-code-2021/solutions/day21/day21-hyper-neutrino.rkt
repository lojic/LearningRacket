#lang racket

;; Port of https://github.com/hyper-neutrino/advent-of-code/blob/main/2021/day21p2.py
(define cache (make-hash))

(define c (for*/list ([ x (in-inclusive-range 1 3) ]
                      [ y (in-inclusive-range 1 3) ]
                      [ z (in-inclusive-range 1 3) ])
            (+ x y z)))

(define (u p0 p1 [s0 0] [s1 0])
  (let* ([ k    (list p0 p1 s0 s1)    ]
         [ pair (hash-ref cache k #f) ])
    (if pair
        pair
        (let ([ oc0 0 ][ oc1 0 ])
          (for ([ r (in-list c) ])
            (let* ([ p0* (+ p0 r) ]
                   [ p0* (if (> p0* 10) (- p0* 10) p0*) ]
                   [ s0* (+ s0 p0*) ])
              (if (>= s0* 21)
                  (set! oc0 (add1 oc0))
                  (let* ([ pair (u p1 p0* s1 s0*) ]
                         [ dy   (car pair)        ]
                         [ dx   (cdr pair)        ])
                    (set! oc0 (+ oc0 dx))
                    (set! oc1 (+ oc1 dy))))))
          (let ([ pair (cons oc0 oc1) ])
            (hash-set! cache k pair)
            pair)))))

(time (u 1 3))
