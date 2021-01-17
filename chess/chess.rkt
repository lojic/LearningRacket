#lang racket

(require "./board-funcs.rkt")
(require "./board.rkt")
(require "./move.rkt")
(require "./movement.rkt")
(require "./piece.rkt")
(require "./san.rkt")

(define (search b max-level)
  (set-board-depth! b 0)
  (alpha-beta b max-level -inf.0 +inf.0))

(define (move-iterator b)
  (generate-moves b)
  (let ([ tmoves (tactical-moves b) ]
        [ thead  (tactical-head b)  ]
        [ ti     0                  ]
        [ qmoves (quiet-moves b)    ]
        [ qhead  (quiet-head b)     ]
        [ qi     0                  ])
    (λ ()
      (cond [ (<= ti thead)
              (set! ti (add1 ti))
              (vector-ref tmoves (sub1 ti)) ]
            [ (<= qi qhead)
              (set! qi (add1 qi))
              (vector-ref qmoves (sub1 qi)) ]
            [ else #f ]))))

(define (alpha-beta b max-level alpha beta)
  (define depth (board-depth b))
  (if (= depth max-level)
      (evaluate b)
      (let ([ maximizing (board-whites-move? b) ]
            [ get-move (move-iterator b) ])
        (let loop ([ alpha alpha ]
                   [ beta  beta  ]
                   [ move  #f    ])
          (let ([ m (get-move) ])
            (if (or (not m) (>= alpha beta))
                ;; No more moves, or alpha >= beta
                (if maximizing
                    ;; If we're at the top level, return move & score;
                    ;; otherwise, just score
                    (if (= depth 0) (cons alpha move) alpha)
                    (if (= depth 0) (cons beta move)  beta))
                (begin
                  (make-move! b m)
                  (let ([ score (alpha-beta b max-level alpha beta) ])
                    (unmake-move! b m)
                    (if maximizing
                        (if (> score alpha)
                            (loop score beta m)
                            (loop alpha beta move))
                        (if (< score beta)
                            (loop alpha score m)
                            (loop alpha beta move)))))))))))

(define (game depth computer-plays-black? [ file-path #f ])
  (define b (create-board))

  (when file-path
    (san-load-file b file-path)
    (print-board b #:full #t))

  (when computer-plays-black?
    (display "Enter move: ")
    (make-move! b (san-move b (read-line)))
    (print-board b #:full #t))

  (let loop ()
    (let* ([ pair (time (search b depth)) ]
           [ score (car pair) ]
           [ m (cdr pair) ])
      (if m
          (begin
            (displayln "")
            (print-move m)
            (displayln "")
            (make-move! b m)
            (print-board b #:full #t)
            (displayln "")
            (display "Enter move: ")
            (displayln "")
            (make-move! b (san-move b (read-line)))
            (print-board b #:full #t)
            (loop))
          (printf "No move!\n")))))

;(game 8 #t "./game.01")
(game 7 #f)
;(game 8)
