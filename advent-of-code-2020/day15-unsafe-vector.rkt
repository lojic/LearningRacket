#lang racket

;; Version using unsafe vector and arithmetic
(require racket/fixnum)
(require racket/unsafe/ops)

(define (run numbers limit)
  (define vec (make-fxvector (* 32 1024  1024) 0))
  (for ([ turn (in-naturals 1)                  ]
        [ num  (in-list (drop-right numbers 1)) ])
    (unsafe-fxvector-set! vec num turn))

  (let loop ([ turn (length numbers) ][ last (last numbers) ])
    (cond [ (unsafe-fx>= turn limit) last ]
          [ else (let* ([ turn (unsafe-fx+ turn 1)                              ]
                        [ num  (let ([ prev-turn (unsafe-fxvector-ref vec last) ])
                                 (if (unsafe-fx> prev-turn 0)
                                     (unsafe-fx- (unsafe-fx- turn 1) prev-turn)
                                     0)) ])
                   (unsafe-fxvector-set! vec last (unsafe-fx- turn 1))
                   (loop turn num)) ])))

(module+ test (require rackunit)
  (check-equal? (run '(12 20 0 6 1 17 7) 2020) 866)
  (check-equal? (run '(12 20 0 6 1 17 7) 30000000) 1437692))
