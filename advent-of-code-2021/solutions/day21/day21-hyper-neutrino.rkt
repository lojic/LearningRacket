#lang racket

;; Port of https://github.com/hyper-neutrino/advent-of-code/blob/main/2021/day21p2.py
(define cache (make-hash))

(define rolls (for*/list ([ x (in-inclusive-range 1 3) ]
                          [ y (in-inclusive-range 1 3) ]
                          [ z (in-inclusive-range 1 3) ])
                (+ x y z)))

(define (play pos0 pos1 [score0 0] [score1 0])
  (let* ([ key  (list pos0 pos1 score0 score1) ]
         [ pair (hash-ref cache key #f)        ])
    (if pair
        pair
        (let ([ wins0 0 ][ wins1 0 ])
          (for ([ roll (in-list rolls) ])
            (let* ([ pos0* (+ pos0 roll) ]
                   [ pos0* (if (> pos0* 10) (- pos0* 10) pos0*) ]
                   [ score0* (+ score0 pos0*) ])
              (if (>= score0* 21)
                  (set! wins0 (add1 wins0))
                  (let* ([ pair         (play pos1 pos0* score1 score0*) ]
                         [ delta-wins1  (car pair)                       ]
                         [ delta-wins0  (cdr pair)                       ])
                    (set! wins0 (+ wins0 delta-wins0))
                    (set! wins1 (+ wins1 delta-wins1))))))
          (let ([ pair (cons wins0 wins1) ])
            (hash-set! cache key pair)
            pair)))))

(time (play 1 3))
