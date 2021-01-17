#lang racket

(require "./board.rkt")
(require "./board-funcs.rkt")
(require "./piece.rkt")
(require "./move.rkt")
(require "./movement.rkt")
(require debug/repl)

(provide san-load-file san-move)

(define (san-load-file b path)
  (printf "Loading file: ~a\n" path)
  (for ([ line (in-list (file->lines path)) ])
    (printf "~a\n" line)
    (let* ([ groups (regexp-match #px"^([0-9]+)[.] ([-a-zQBNRO0-9]+) ([-a-zQBNRO0-9]+)$" line) ]
           [ n  (second groups) ]
           [ wstr (third groups) ]
           [ bstr (fourth groups) ])
      (let ([ m (san-move b wstr) ])
        (printf "~a's move: " (if (board-whites-move? b) "White" "Black"))
        (print-move m)
        (make-move! b m))
      (let ([ m (san-move b bstr) ])
        (printf "~a's move: " (if (board-whites-move? b) "White" "Black"))
        (print-move m)
        (make-move! b m))
      (printf "\n"))))

(define (san-move b str)
  (define is-white? (board-whites-move? b))
  (cond [ (regexp-match? #px"^[a-h][1-8][QBNR]?$" str)
          (san-pawn-push b is-white? str) ]
        [ (regexp-match? #px"^[a-h]x[a-h][1-8]$" str)
          (san-pawn-capture b is-white? str) ]
        [ (regexp-match? #px"^[a-h][1-8][-x][a-h][1-8]$" str)
          (san-piece-move b is-white? str) ]
        [ (string=? str "O-O")
          (san-castle-kingside b is-white?) ]
        [ (string=? str "O-O-O")
          (san-castle-queenside b is-white?) ]))

(define (san-castle-kingside b is-white?)
  (let-values ([ (src-idx dst-idx)
                 (if is-white?
                     (values (idx-from-pos "e1") (idx-from-pos "g1"))
                     (values (idx-from-pos "e8") (idx-from-pos "g8"))) ])
    (create-move (bytes-ref (board-squares b) src-idx) src-idx dst-idx #:is-castle-kingside? #t)))

(define (san-castle-queenside b is-white?)
  (let-values ([ (src-idx dst-idx)
                 (if is-white?
                     (values (idx-from-pos "e1") (idx-from-pos "c1"))
                     (values (idx-from-pos "e8") (idx-from-pos "c8"))) ])
    (create-move (bytes-ref (board-squares b) src-idx) src-idx dst-idx #:is-castle-queenside? #t)))

(define (san-pawn-capture b is-white? str)
  (let* ([ groups (regexp-match #px"^([a-h])x([a-h])([1-8])$" str) ]
         [ source-file (second groups) ]
         [ target-file (third groups) ]
         [ target-rank (string->number (fourth groups)) ]
         [ source-rank (if is-white?
                           (sub1 target-rank)
                           (add1 target-rank)) ]
         [ src-idx (idx-from-pos (format "~a~a" source-file source-rank)) ]
         [ dst-idx (idx-from-pos (format "~a~a" target-file target-rank)) ]
         [ piece (bytes-ref (board-squares b) src-idx) ]
         [ target (bytes-ref (board-squares b) dst-idx) ])
    (if (= target empty-square)
        ;; En passant capture
        (let* ([ cap-idx (idx-from-pos (format "~a~a" target-file source-rank)) ]
               [ cap-piece (bytes-ref (board-squares b) cap-idx) ])
          (create-move piece src-idx dst-idx #:captured-piece cap-piece #:is-ep-capture? #t))
        ;; Else, regular capture
        (create-move piece src-idx dst-idx #:captured-piece target))))

(define (san-pawn-push b is-white? str)
  (let* ([ groups   (regexp-match #px"^([a-h])([1-8])([QBNR])?$" str) ]
         [ file     (second groups)                                   ]
         [ rank     (string->number (third groups))                   ]
         [ promoted (fourth groups)                                   ]
         [ pos      (format "~a~a" file rank)                         ]
         [ idx      (idx-from-pos pos)                                ]
         [ i1       (+ idx (if is-white? south north))                ]
         [ i2       (+ i1  (if is-white? south north))                ])
    (if (= (bytes-ref (board-squares b) i1) empty-square)
        ;; One square before is empty, so must be double push
        (let ([ piece (bytes-ref (board-squares b) i2) ])
          (create-move piece i2 idx))
        ;; Otherwise, single push
        (let ([ piece (bytes-ref (board-squares b) i1) ])
          (if promoted
              ;; Pawn promotion
              (if is-white?
                  (san-white-pawn-promotion piece i1 idx rank promoted)
                  (san-black-pawn-promotion piece i1 idx rank promoted))
              ;; Pawn push
              (create-move piece i1 idx))))))

(define (san-white-pawn-promotion piece src-idx dst-idx rank letter)
  (if (= rank 8)
      (create-move piece src-idx dst-idx
                   #:promoted-piece (match letter
                                      [ "Q" white-queen  ]
                                      [ "B" white-bishop ]
                                      [ "N" white-knight ]
                                      [ "R" white-rook   ]))
      (error "invalid white pawn promotion")))

(define (san-black-pawn-promotion piece src-idx dst-idx rank letter)
  (if (= rank 1)
      (create-move piece src-idx dst-idx
                   #:promoted-piece (match letter
                                      [ "Q" black-queen  ]
                                      [ "B" black-bishop ]
                                      [ "N" black-knight ]
                                      [ "R" black-rook   ]))
      (error "invalid black pawn promotion")))

(define (san-piece-move b is-white? str)
  (let* ([ groups (regexp-match #px"^([a-h][1-8])([-x])([a-h][1-8])$" str) ]
         [ src-pos     (second groups)                       ]
         [ src-idx     (idx-from-pos src-pos)                ]
         [ src         (bytes-ref (board-squares b) src-idx) ]
         [ cap-or-move (third groups)                        ]
         [ dst-pos     (fourth groups)                       ]
         [ dst-idx     (idx-from-pos dst-pos)                ]
         [ dst         (bytes-ref (board-squares b) dst-idx) ])
    (if (or (and (string=? cap-or-move "x")
                 (is-piece? dst))
            (and (string=? cap-or-move "-")
                 (= dst empty-square)))
        (create-move src src-idx dst-idx
                     #:captured-piece (if (= dst empty-square)
                                          #f
                                          dst))
        (begin
          (debug-repl)
          (error (format "invalid move spec: ~a" str))))))
