#lang racket
(require "../advent.rkt" data/heap/unsafe)

;; I needed hints to finish this one. I figured out the growth was
;; x^2, but I needed the hints of the cycle being equal to the grid
;; dimension e.g. 131, and the fact that the first map was filled w/in
;; 65 steps (i.e. # of steps to the edge). With that knowledge, I just
;; needed to know the the number of plots reached at step 65, and a
;; couple more cycles, 196, 327. So this program just prints out a
;; series of steps and plots reached, and the answer was found using a
;; spreadsheet :)

(define-values (grid dim start)
  (let* ([ lines (parse-aoc 21 string->list) ]
         [ dim   (length lines)              ]
         [ grid  (grid->hash lines)          ])
    (values grid dim (findf (λ (pos)
                              (char=? #\S (hash-ref grid pos)))
                            (hash-keys grid)))))

(struct state (steps pos page) #:transparent)

(define (get-candidates obj)
  (let ([ steps (state-steps obj) ]
        [ pos   (state-pos obj)   ]
        [ page  (state-page obj)  ])
    (~> '(1 -1 +i -i)
        (map (λ (dir)
               (let* ([ pos*   (+ pos dir)  ]
                      [ steps* (add1 steps) ])
                 (if (and (<= 0 (real-part pos*) (sub1 dim))
                          (<= 0 (imag-part pos*) (sub1 dim)))
                     (state steps* pos* page)
                     (state steps* (- pos* (* dim dir)) (+ page dir)))))
             _)
        (filter (λ (obj)
                  (not (char=? #\# (hash-ref grid (state-pos obj)))))
                _))))

(define (solve)
  (let ([ seen  (mutable-set) ]
        [ queue (make-heap (λ (a b)
                             (<= (state-steps a) (state-steps b)))) ]
        [ dist 0 ])
    (heap-add! queue (state 0 start 0))

    (let bfs ()
      (let ([ obj (heap-min queue) ])
        (heap-remove-min! queue)

        (when (> (state-steps obj) dist)
          (printf "steps = ~a, plots = ~a\n" dist (count (λ (o) (= dist (state-steps o))) (set->list seen))))
          (set! dist (state-steps obj))

        (cond [ (set-member? seen obj)  (bfs) ]
              [ else (set-add! seen obj)
                     (for ([ obj (get-candidates obj) ])
                       (heap-add! queue obj))
                     (bfs) ])))))

(solve)
