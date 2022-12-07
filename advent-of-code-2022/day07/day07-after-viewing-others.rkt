#lang racket
(require "../advent.rkt")

(define in  (parse-aoc 7 atoms))
(define hsh (make-hash))
(define seen (make-hash))
(define cwd #f)

(define (add-file! size [cwd cwd])
  (hash-set! hsh cwd (+ size (hash-ref hsh cwd 0)))
  (when (not (null? cwd))
    (add-file! size (cdr cwd))))

(for ([ lst (in-list in) ])
  (match lst
    [ (list "$" _)         (void) ]
    [ (list "dir" _)       (void) ]
    [ (list "$" "cd" "..") (set! cwd (drop cwd 1)) ]
    [ (list "$" "cd" dir)  (if (string=? "/" dir)
                               (set! cwd '())
                               (set! cwd (cons dir cwd))) ]
    [ (list size name)     (let ([ key (cons name cwd) ])
                             (when (not (hash-has-key? seen key))
                               (hash-set! seen key #t)
                               (add-file! size))) ]))

(define sizes (~> (hash->list hsh) (map cdr _)))

(list-sum (filter (curry > 100001) sizes))                         ; Part 1

(findf (curry < (+ -40000000 (hash-ref hsh '() ))) (sort sizes <)) ; Part 2
