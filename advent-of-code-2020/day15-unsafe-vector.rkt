#lang racket

;; Version using unsafe vector and arithmetic
(require racket/fixnum)

(define (run numbers limit)
  (define vec (make-fxvector (* 32 1024  1024) 0))
  (for ([ turn (in-naturals 1)                  ]
        [ num  (in-list (drop-right numbers 1)) ])
    (fxvector-set! vec num turn))

  (let loop ([ turn (length numbers) ][ last (last numbers) ])
    (cond [ (fx>= turn limit) last ]
          [ else (let* ([ turn (fx+ turn 1)                              ]
                        [ num  (let ([ prev-turn (fxvector-ref vec last) ])
                                 (if (fx> prev-turn 0)
                                     (fx- (fx- turn 1) prev-turn)
                                     0)) ])
                   (fxvector-set! vec last (fx- turn 1))
                   (loop turn num)) ])))

(module+ test (require rackunit)
  (check-equal? (run '(12 20 0 6 1 17 7) 2020) 866)
  (check-equal? (run '(12 20 0 6 1 17 7) 30000000) 1437692))
