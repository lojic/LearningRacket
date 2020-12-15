#lang racket

;; Mutable vector version
(define (run numbers limit)
  (define vec (make-vector (* 32 1024  1024) #f))
  (for ([ turn (in-naturals 1)                  ]
        [ num  (in-list (drop-right numbers 1)) ])
    (vector-set! vec num turn))

  (let loop ([ turn (length numbers) ][ last (last numbers) ])
    (cond [ (>= turn limit) last ]
          [ else (let* ([ turn (add1 turn)                              ]
                        [ num  (let ([ prev-turn (vector-ref vec last) ])
                                 (if prev-turn
                                     (- (sub1 turn) prev-turn)
                                     0)) ])
                   (vector-set! vec last (sub1 turn))
                   (loop turn num)) ])))

(module+ test (require rackunit)
  (check-equal? (run '(12 20 0 6 1 17 7) 2020) 866)
  (check-equal? (run '(12 20 0 6 1 17 7) 30000000) 1437692))
