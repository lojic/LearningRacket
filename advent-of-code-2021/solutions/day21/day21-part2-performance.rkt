#lang racket

;; Was able to get the time down from 12,000 ms to 65 ms with this version.

(require racket/performance-hint)

(struct player     (position score)           #:transparent)
(struct game-state (white black whites-move?) #:transparent)

(define cache        (make-hash))
(define distribution (vector-immutable 0 0 0 1 3 6 7 6 3 1))
(define rolls        '(3 4 5 6 7 8 9))

(define (play white black [ whites-move? #t ][ num-leaves 1 ])
  (cond [ (>= (player-score white) 21) (values num-leaves 0) ]
        [ (>= (player-score black) 21) (values 0 num-leaves) ]
        [ else (let* ([ state (game-state white black whites-move?) ]
                      [ wins  (hash-ref cache state #f)             ])
                 (if wins
                     (values (* num-leaves (car wins)) (* num-leaves (cdr wins)))

                     (let loop ([ rolls rolls ][ white-score 0 ][ black-score 0 ])
                       (if (null? rolls)
                           (begin
                             (hash-set! cache
                                        (game-state white black whites-move?)
                                        (cons (/ white-score num-leaves) (/ black-score num-leaves)))
                             (values white-score black-score))
                           (let* ([ roll   (car rolls)                               ]
                                  [ leaves (vector-ref distribution roll)   ]
                                  [ white  (if whites-move? (turn white roll) white) ]
                                  [ black  (if whites-move? black (turn black roll)) ])
                             (let-values ([ (w b) (play white
                                                        black
                                                        (not whites-move?)
                                                        (* num-leaves leaves)) ])
                               (loop (cdr rolls) (+ white-score w) (+ black-score b)))))))) ]))

(define-inline (turn the-player move)
  (let* ([ pos (+ (player-position the-player) move) ]
         [ pos (if (> pos 10) (- pos 10) pos) ])
    (struct-copy player the-player
                 [ position pos ]
                 [ score (+ (player-score the-player) pos) ])))

(let-values ([ (w _) (time (play (player 1 0) (player 3 0))) ])
  (= 48868319769358 ))
