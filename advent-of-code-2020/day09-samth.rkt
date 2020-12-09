#lang racket

;; An efficient version by Sam Tobin-Hochstadt, lightly modified by me.

(define input (time (list->vector (file->list "day09.txt"))))

(define (part1 input i)
  (let ([ v (vector-ref input (+ i 25)) ])
    (if (for/or ([ k (in-combinations (vector->list (vector-copy input i (+ i 25))) 2) ])
          (= v (+ (car k) (cadr k))))
        (part1 input (add1 i))
        v)))

(define (part2 input val)
  (let helper ([ i 0 ][ val val ][ num 0 ][ sum 0 ])
    (cond [ (< sum val) (helper i val (add1 num) (+ sum (vector-ref input (+ i num)))) ]
          [ (> sum val) (helper (+ i 1) val (sub1 num) (- sum (vector-ref input i)))   ]
          [ else (let ([ slice (vector->list (vector-copy input i (+ i num))) ])
                   (+ (apply min slice) (apply max slice))) ])))

(define val (time (part1 input 0)))
val
(time (part2 input val))
