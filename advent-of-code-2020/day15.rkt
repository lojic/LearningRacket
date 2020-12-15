#lang racket

(define (run numbers limit)
  (let loop ([ turn (length numbers) ]
             [ hsh  (for/hash ([ turn (in-naturals 1)                  ]
                               [ num  (in-list (drop-right numbers 1)) ])
                      (values num turn)) ]
             [ last (last numbers) ])
    (cond [ (>= turn limit) last ]
          [ else (let* ([ turn (add1 turn)                              ]
                        [ num  (let ([ prev-turn (hash-ref hsh last #f) ])
                                 (if prev-turn
                                     (- (sub1 turn) prev-turn)
                                     0)) ])
                   (loop turn (hash-set hsh last (sub1 turn)) num)) ])))

(module+ test (require rackunit)
  (check-equal? (run '(12 20 0 6 1 17 7) 2020) 866)
  (check-equal? (run '(12 20 0 6 1 17 7) 30000000) 1437692))
