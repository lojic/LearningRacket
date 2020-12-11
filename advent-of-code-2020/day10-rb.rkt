#lang racket

;; Version inspired by a Ruby version I saw.

(define arr (list->vector
             (let ([ lst (sort (file->list "day10.txt") <) ])
               (append (cons 0 lst) (list (+ 3 (last lst)))))))

;; Part 1
(define diffs (for/list ([ (x y) (in-parallel arr (vector-take-right arr (sub1 (vector-length arr)))) ])
                (- y x)))
(displayln (* (count (curry = 1) diffs) (count (curry = 3) diffs)))

;; Part 2
(define memo (make-hash))
(define end (sub1 (vector-length arr)))
(define (get val)
  (cond [ (not (vector-member val arr)) 0 ]
        [ (= val (vector-ref arr end))  1 ]
        [ else (or (hash-ref memo val #f)
                   (let ([ x (+ (get (+ val 1)) (get (+ val 2)) (get (+ val 3))) ])
                     (hash-set! memo val x)
                     x)) ]))
(displayln (get 0))
