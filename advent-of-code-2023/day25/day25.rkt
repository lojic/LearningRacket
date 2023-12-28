#lang racket
(require "../advent.rkt" graph)

(define g (undirected-graph (for/fold ([ edges '() ])
                                      ([ lst (in-list (parse-aoc 25 words)) ])
                              (let ([ v1 (car lst) ])
                                (for/fold ([ edges edges ])
                                          ([ v2 (in-list (cdr lst)) ])
                                  (cons (list v1 v2) edges))))))

(define (max-f source sink)
  (count (λ (l)
           (string=? source (car l)))
         (hash-keys (maxflow g source sink))))

(define (part1)
  (match-let ([ (list v1 vertices ...) (get-vertices g) ])
    (let loop ([ vertices vertices ][ S (list v1) ][ T '() ])
      (if (null? vertices)
          (* (length S) (length T))
          (let ([ v2 (car vertices) ])
            (if (> (max-f v1 v2) 3)
                (loop (cdr vertices) (cons v2 S) T)
                (loop (cdr vertices) S (cons v2 T))))))))

;; Tests --------------------------------------------------------------------------------------

(check-equal? (part1) 592171)
