#lang racket

;; This version was left "as is" because it was the first thing I
;; thought of (after stewing on the problem description for a long
;; time), and it worked the very first time, as soon as I finished
;; coding it which is a rare experience for me in this year's Advent
;; of Code!

;; I'll use another version for cleanup & performance tuning, but this
;; version is "going on the wall" so to speak :)

;; Takes about 12 seconds to count the wins.

(require threading)

(struct player (position score))

(define distribution (~>> (for*/list ([ i (in-inclusive-range 1 3) ]
                                      [ j (in-inclusive-range 1 3) ]
                                      [ k (in-inclusive-range 1 3) ])
                            (+ i j k))
                          (group-by identity)
                          (map (λ (l) (cons (car l) (length l))))
                          make-immutable-hash))

(define rolls (hash-keys distribution))

(define (count-leafs moves)
  (if (null? moves)
      1
      (* (hash-ref distribution (car moves))
         (count-leafs (cdr moves)))))

(define (play whites-move? moves white black)
  (cond [ (>= (player-score white) 21) (values (count-leafs moves) 0) ]
        [ (>= (player-score black) 21) (values 0 (count-leafs moves)) ]
        [ else
          (let loop ([ rolls rolls ][ white-score 0 ][ black-score 0 ])
            (if (null? rolls)
                (values white-score black-score)
                (let ([ roll (car rolls) ])
                  (if whites-move?
                      (let ([ white (turn white roll) ])
                        (let-values ([ (w b) (play (not whites-move?) (cons roll moves) white black) ])
                          (loop (cdr rolls) (+ white-score w) (+ black-score b))))
                      (let ([ black (turn black roll) ])
                        (let-values ([ (w b) (play (not whites-move?) (cons roll moves) white black) ])
                          (loop (cdr rolls) (+ white-score w) (+ black-score b)))))))) ]))

(define (turn the-player move)
  (let* ([ pos (+ (player-position the-player)
                  move) ]
         [ pos (if (> pos 10) (- pos 10) pos) ])
    (struct-copy player the-player
                 [ position pos ]
                 [ score (+ (player-score the-player) pos) ])))

(time (play #t '() (player 1 0) (player 3 0)))
