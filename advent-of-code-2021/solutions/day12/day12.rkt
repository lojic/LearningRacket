#lang racket

;; A recursive, functional version using the graph module for neighbors.

(require graph threading "../../advent/advent.rkt")

(define (solve g neighbor?)
  (length (get-paths g neighbor?)))

(define (get-paths g neighbor? [ path '("start") ])
  (define cave (first path))
  (cond [ (string=? cave "end") '(#t) ]
        [ else (for/fold ([ paths '() ])
                         ([ neighbor (filter (curry neighbor? path)
                                             (get-neighbors g cave)) ])
                 (append (get-paths g
                                    neighbor?
                                    (cons neighbor path))
                         paths)) ]))

(define (first-revisit? path)
  (~>> (filter small? path)
       (group-by identity)
       (count (λ (l) (> (length l) 1)))
       zero?))

(define (parse fname)
  (~>> (file->lines fname)
       (map (curryr string-split "-"))
       undirected-graph))

(define (valid-neighbor-1? path cave)
  (or (large? cave)
      (not (member cave path))))

(define (valid-neighbor-2? path cave)
  (or (valid-neighbor-1? path cave)
      (and (non-terminal? cave)
           (first-revisit? path))))

(define large?        string-upper-case?)
(define non-terminal? (λ (v) (not (member v '("start" "end")))))
(define small?        (λ (v) (and (not (large? v)) (non-terminal? v))))

;; Tests --------------------------------------------------------------------------------------

(module+ test
  (require rackunit)
  (let ([ g (parse "day12.txt") ])
    (check-equal? (solve g valid-neighbor-1?) 3887)
    (check-equal? (solve g valid-neighbor-2?) 104834)))
