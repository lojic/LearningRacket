#lang racket

;; This version w/ a mutable hash table runs in about 40% of the time
;; as the immutable hash version
(define (run numbers limit)
  (define hsh (make-hash))
  (for ([ turn (in-naturals 1)                  ]
        [ num  (in-list (drop-right numbers 1)) ])
    (hash-set! hsh num turn))

  (let loop ([ turn (length numbers) ][ last (last numbers) ])
    (cond [ (>= turn limit) last ]
          [ else (let* ([ turn (add1 turn)                              ]
                        [ num  (let ([ prev-turn (hash-ref hsh last #f) ])
                                 (if prev-turn
                                     (- (sub1 turn) prev-turn)
                                     0)) ])
                   (hash-set! hsh last (sub1 turn))
                   (loop turn num)) ])))

(module+ test (require rackunit)
  (check-equal? (run '(12 20 0 6 1 17 7) 2020) 866)
  (check-equal? (run '(12 20 0 6 1 17 7) 30000000) 1437692))
