#lang racket
(require "../advent.rkt" data/heap/unsafe)

(define-values (N grid start)
  (let ([ grid (grid->hash (parse-aoc 21 string->list)) ])
    (values 64 grid (findf (λ (pos)
                             (char=? #\S (hash-ref grid pos)))
                           (hash-keys grid)))))

(define (solve)
  (let ([ seen  (mutable-set) ]
        [ queue (make-heap (λ (a b)
                             (<= (car a) (car b)))) ])
    (heap-add! queue (cons 0 start))

    (let bfs ()
      (let ([ obj (heap-min queue) ])
        (heap-remove-min! queue)
        (cond [ (> (car obj) N) seen  ]
              [ (set-member? seen obj)  (bfs) ]
              [ else (set-add! seen obj)
                     (let ([ steps* (add1 (car obj)) ])
                       (for ([ pos (filter (λ (pos*)
                                             (not (char=? #\#(hash-ref grid pos* #\#))))
                                           (map (λ (dir)
                                                  (+ (cdr obj) dir))
                                                '(1 -1 +i -i))) ])
                         (heap-add! queue (cons steps* pos)))
                       (bfs)) ])))))

(define (part1)
  (~> (set->list (solve))
      (count (λ (o) (= N (car o))) _)))

;; Tests --------------------------------------------------------------------------------------

(check-equal? (part1) 3639)
