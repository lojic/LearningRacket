#lang racket
(require "../advent.rkt")

(struct stone (px py pz vx vy vz) #:transparent)

(define stones (~> (parse-aoc 24 numbers)
                   (map (curry apply stone) _)))

;; https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
(define (intersection s1 s2 ignored-axis)
  (cond [ (eq? ignored-axis 'z)
          (let* ([ x1 (stone-px s1)        ]
                 [ y1 (stone-py s1)        ]
                 [ x2 (+ x1 (stone-vx s1)) ]
                 [ y2 (+ y1 (stone-vy s1)) ]
                 [ x3 (stone-px s2)        ]
                 [ y3 (stone-py s2)        ]
                 [ x4 (+ x3 (stone-vx s2)) ]
                 [ y4 (+ y3 (stone-vy s2)) ]
                 [ denom (- (* (- x1 x2)
                               (- y3 y4))
                            (* (- y1 y2)
                               (- x3 x4))) ])
            (if (zero? denom)
                #f
                (let ([ numx (- (* (- (* x1 y2)
                                      (* y1 x2))
                                   (- x3 x4))
                                (* (- x1 x2)
                                   (- (* x3 y4)
                                      (* y3 x4)))) ]
                      [ numy (- (* (- (* x1 y2)
                                      (* y1 x2))
                                   (- y3 y4))
                                (* (- y1 y2)
                                   (- (* x3 y4)
                                      (* y3 x4)))) ])
                  (cons (/ numx denom) (/ numy denom)))))]))

(define (intersections-in-area t1 t2 stones)
  (define (not-in-past? s p)
    (let ([ pvx (- (car p) (stone-px s)) ]
          [ pvy (- (cdr p) (stone-py s)) ]
          [ vx (stone-vx s) ]
          [ vy (stone-vy s) ])
      (and (positive? (* pvx vx))
           (positive? (* pvy vy)))))

  (define (in-area? t1 t2 p)
    (and (<= t1 (car p) t2)
         (<= t1 (cdr p) t2)))

  (define (intersects-test-area? t1 t2 s)
    (define (test? p) (and p (in-area? t1 t2 p)))

    (or (test? (intersection s (stone t1 t1 t1 0 1 0) 'z))
        (test? (intersection s (stone t2 t2 t2 -1 0 0) 'z))
        (test? (intersection s (stone t2 t2 t2 0 -1 0) 'z))
        (test? (intersection s (stone t1 t1 t1 1 0 0) 'z))))

  (let loop ([ lst (filter (λ (s) (intersects-test-area? t1 t2 s)) stones) ]
             [ intersections (set) ])
    (if (null? lst)
        intersections
        (let ([ s (car lst) ])
          (loop (cdr lst)
                (for/fold ([ intersections intersections ])
                          ([ s* (in-list (cdr lst)) ])
                  (let ([ p (intersection s s* 'z) ])
                    (if (and p
                             (in-area? t1 t2 p)
                             (not-in-past? s p)
                             (not-in-past? s* p))
                        (set-add intersections p)
                        intersections))))))))

(define (part1)
  (set-count (intersections-in-area 200000000000000 400000000000000 stones)))

;; Tests --------------------------------------------------------------------------------------

(check-equal? (part1) 13754)
