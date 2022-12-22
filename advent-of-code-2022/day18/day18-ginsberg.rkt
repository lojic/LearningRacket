;; Port of Todd Ginsberg's solution for Advent of Code 2022 Day 18
;; https://github.com/tginsberg/advent-2022-kotlin/blob/main/src/main/kotlin/com/ginsberg/advent2022/Day18.kt

#lang racket
(require "./ginsberg-support.rkt")

(define points (~> input (map point3d-of _) (to-set _)))

(define (solve-part1)
  (sum-of points (λ (point)
                   (- 6 (~> (cardinal-neighbors point)
                            (count (λ (neighbor)
                                     (in? neighbor points)) _))))))

(define (solve-part2)
  (let* ([ x-range     (range-of points it.x) ]
         [ y-range     (range-of points it.y) ]
         [ z-range     (range-of points it.z) ]

         [ que         (add (make-queue) (point3d (car x-range) (car y-range) (car z-range))) ]
         [ seen        (mutable-set) ]
         [ sides-found 0             ])
    (for ([ look-next (in-queue que) ])
      (when (not (in? look-next seen))
        (for ([ neighbor (~> (cardinal-neighbors look-next)
                             (filter (λ (p) (and (in-range? (it.x p) x-range)
                                                 (in-range? (it.y p) y-range)
                                                 (in-range? (it.z p) z-range))) _)) ])
          (set-add! seen look-next)
          (cond [ (in? neighbor points) (set! sides-found (add1 sides-found)) ]
                [ else (enqueue! que neighbor) ]))))

    sides-found))

(define (range-of st fun)
  (cons (sub1 (min-of st fun)) (add1 (max-of st fun))))
