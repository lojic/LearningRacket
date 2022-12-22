#lang racket

(require "../advent.rkt"
         data/queue)

(provide add
         cardinal-neighbors
         input
         in?
         in-range?
         it-x
         it-y
         it-z
         max-of
         min-of
         point3d-of
         sum-of
         to-set
         (struct-out point3d)
         (all-from-out "../advent.rkt")
         (all-from-out data/queue))

(struct point3d (x y z) #:transparent)
(define input (parse-aoc 18 numbers))

(define (add q val)
  (enqueue! q val)
  q)

(define (cardinal-neighbors point)
  (let ([ x (point3d-x point) ]
        [ y (point3d-y point) ]
        [ z (point3d-z point) ])
    (~> (list (list (add1 x) y z)
              (list (sub1 x) y z)
              (list x (add1 y) z)
              (list x (sub1 y) z)
              (list x y (add1 z))
              (list x y (sub1 z)))
        (map point3d-of _))))

(define in? (flip set-member?))

(define (in-range? val pair)
  (and (>= val (car pair))
       (<= val (cdr pair))))

(define it-x point3d-x)
(define it-y point3d-y)
(define it-z point3d-z)

(define (min-of st fun) (list-min (map fun (set->list st))))
(define (max-of st fun) (list-max (map fun (set->list st))))

(define (point3d-of lst)
  (match-let ([ (list x y z) lst ])
    (point3d x y z)))

(define (sum-of lst fun)
  (for/sum ([ it (in-set lst) ])
    (fun it)))

(define to-set list->set)
