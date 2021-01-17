#lang racket
(require threading)

; Task List
; * Special check heuristics
;   - Check by queen, rook bishop - try capture, interposing or moving king
;   - Check by knight - try capture or moving king
; * Think on opponent's time
; * import board from displayed board
; * show score of manually chosen moves for debugging
; * piece value: index into table vs. function call
; * profiling
; * Alpha beta pruning
; * castling
; * en passant
; * pawn promotion (assume queen)
; * discovered check (maybe check via king sliding to opposite color biship/rook/queen?)
; * official notation for moves
; * better static evaluation:
;   - material
;   - piece-square tables
;   - mobility
;   - center control
;   - king safety
; * progressive deepening & clock usage
; * parallelization
; * allow two versions to play many games and determine the better version
; * heuristic continuation
; * general optimizations

;Why not c2 to c3 here (white to move)?
;r _ _ q _ k _ r
;p p _ _ _ p p p
;_ _ p _ _ _ _ _
;_ _ _ p p _ _ _
;_ b _ n n _ b _
;_ P _ _ _ _ _ _
;P B P P P P P P
;R N _ Q K B N R

;; NOTE:  10x12 https://www.chessprogramming.org/10x12_Board

; Initial board represented by a ByteString of 12x12=144 elements.
; Index 0 is at lower left. Index 143 is at upper right.
;   0 0  0  0  0  0  0  0  0  0 0 0
;   0 0  0  0  0  0  0  0  0  0 0 0
; 8 0 0 10 08 09 11 12 09 08 10 0 0
; 7 0 0 07 07 07 07 07 07 07 07 0 0
; 6 0 0 13 13 13 13 13 13 13 13 0 0
; 5 0 0 13 13 13 13 13 13 13 13 0 0
; 4 0 0 13 13 13 13 13 13 13 13 0 0
; 3 0 0 13 13 13 13 13 13 13 13 0 0
; 2 0 0 01 01 01 01 01 01 01 01 0 0
; 1 0 0 04 02 03 05 06 03 02 04 0 0
;   0 0  0  0  0  0  0  0  0  0 0 0
;   0 0  0  0  0  0  0  0  0  0 0 0
;        a  b  c  d  e  f  g  h
;
; By having a 2-square fringe of zeros at the edge, it's easier
; to detect (in)valid moves, especially for knights.

(define board-length 144)

(define max-score 1000000.0)
(define min-score -1000000.0)

; Represent pieces using a byte value (0 to 255). White pieces are >= 1 && < 7,
; black pieces are >= 7 && < 13
(define empty        13)

(define white-king   6)
(define white-queen  5)
(define white-rook   4)
(define white-bishop 3)
(define white-knight 2)
(define white-pawn   1)

(define black-king   12)
(define black-queen  11)
(define black-rook   10)
(define black-bishop 9)
(define black-knight 8)
(define black-pawn   7)

; Directions
(define north       12)
(define north-east  13)
(define east         1)
(define south-east -11)
(define south      -12)
(define south-west -13)
(define west        -1)
(define north-west  11)

; Return the file for a particular board index
; (idx->file 40) -> 'c
(define (idx->file idx)
  (match (remainder idx 12)
    [ 2 'a  ]
    [ 3 'b  ]
    [ 4 'c  ]
    [ 5 'd  ]
    [ 6 'e  ]
    [ 7 'f  ]
    [ 8 'g  ]
    [ 9 'h  ]
    [ _ '() ]))

; Return the rank for a particular board index
; (idx->rank 40) -> 2
(define (idx->rank idx)
  (- (quotient idx 12) 1))

; Convert a rank into an index where a is 0.
; Expects a byte, not a character
; (rank->idx (char->integer #\b)) -> 1
(define (rank->idx rank)
  (- rank (char->integer #\a)))

; Convert a file number into an index 1 is 0.
; Expects a byte, not a character
; (file->idx (char->integer #\2)) -> 1
(define (file->idx file)
  (- file (char->integer #\1)))

; Convert a position, such as b2, to an index into the board byte string
; (pos->idx #"b2") -> 39
(define (pos->idx pos)
  (let ([rank-idx (rank->idx (bytes-ref pos 0))]
        [file-idx (file->idx (bytes-ref pos 1))])
    (+ (* 12 (+ file-idx 2))
       (+ 2 rank-idx))))

; Convert an index, e.g. 39, into a position, e.g. #"b2"
; (idx->pos 39) => #"b2"
(define (idx->pos idx)
  (let-values ([(file rank) (quotient/remainder idx 12)])
    (let ([rank-byte (bytes-ref #"00abcdefgh" rank)]
          [file-byte (bytes-ref #"0012345678" file)])
      (bytes rank-byte file-byte))))

; Place piece at position pos on board
(define (board-set! board pos piece)
  (bytes-set! board (pos->idx pos) piece))

; Create initial board
(define board (let ([b (make-bytes board-length 0)]
                    [moves `((#"a1" . ,white-rook)
                             (#"b1" . ,white-knight)
                             (#"c1" . ,white-bishop)
                             (#"d1" . ,white-queen)
                             (#"e1" . ,white-king)
                             (#"f1" . ,white-bishop)
                             (#"g1" . ,white-knight)
                             (#"h1" . ,white-rook)
                             (#"a2" . ,white-pawn)
                             (#"b2" . ,white-pawn)
                             (#"c2" . ,white-pawn)
                             (#"d2" . ,white-pawn)
                             (#"e2" . ,white-pawn)
                             (#"f2" . ,white-pawn)
                             (#"g2" . ,white-pawn)
                             (#"h2" . ,white-pawn)
                             (#"a3" . ,empty)
                             (#"b3" . ,empty)
                             (#"c3" . ,empty)
                             (#"d3" . ,empty)
                             (#"e3" . ,empty)
                             (#"f3" . ,empty)
                             (#"g3" . ,empty)
                             (#"h3" . ,empty)
                             (#"a4" . ,empty)
                             (#"b4" . ,empty)
                             (#"c4" . ,empty)
                             (#"d4" . ,empty)
                             (#"e4" . ,empty)
                             (#"f4" . ,empty)
                             (#"g4" . ,empty)
                             (#"h4" . ,empty)
                             (#"a5" . ,empty)
                             (#"b5" . ,empty)
                             (#"c5" . ,empty)
                             (#"d5" . ,empty)
                             (#"e5" . ,empty)
                             (#"f5" . ,empty)
                             (#"g5" . ,empty)
                             (#"h5" . ,empty)
                             (#"a6" . ,empty)
                             (#"b6" . ,empty)
                             (#"c6" . ,empty)
                             (#"d6" . ,empty)
                             (#"e6" . ,empty)
                             (#"f6" . ,empty)
                             (#"g6" . ,empty)
                             (#"h6" . ,empty)
                             (#"a7" . ,black-pawn)
                             (#"b7" . ,black-pawn)
                             (#"c7" . ,black-pawn)
                             (#"d7" . ,black-pawn)
                             (#"e7" . ,black-pawn)
                             (#"f7" . ,black-pawn)
                             (#"g7" . ,black-pawn)
                             (#"h7" . ,black-pawn)
                             (#"a8" . ,black-rook)
                             (#"b8" . ,black-knight)
                             (#"c8" . ,black-bishop)
                             (#"d8" . ,black-queen)
                             (#"e8" . ,black-king)
                             (#"f8" . ,black-bishop)
                             (#"g8" . ,black-knight)
                             (#"h8" . ,black-rook))])
                (for ([pair moves])
                  (board-set! b (car pair) (cdr pair)))
                b))

; Return the value of the specified piece
(define (piece-value piece)
  (match piece
    [ (== white-king)   1000000 ]
    [ (== white-queen)   9 ]
    [ (== white-rook)    5 ]
    [ (== white-bishop)  3 ]
    [ (== white-knight)  3 ]
    [ (== white-pawn)    1 ]
    [ (== black-king)  -1000000 ]
    [ (== black-queen)  -9 ]
    [ (== black-rook)   -5 ]
    [ (== black-bishop) -3 ]
    [ (== black-knight) -3 ]
    [ (== black-pawn)   -1 ]
    [ _                  0 ]))

; Return a list of indices to which the specified piece can move
(define (valid-targets board idx)
  (let ([piece (bytes-ref board idx)])
    (cond [(= piece white-king)   (valid-king-targets   board idx is-black-piece?) ]
          [(= piece white-queen)  (valid-queen-targets  board idx is-black-piece?) ]
          [(= piece white-rook)   (valid-rook-targets   board idx is-black-piece?) ]
          [(= piece white-bishop) (valid-bishop-targets board idx is-black-piece?) ]
          [(= piece white-knight) (valid-knight-targets board idx is-black-piece?) ]
          [(= piece white-pawn)   (valid-pawn-targets   board idx is-black-piece?) ]
          [(= piece black-king)   (valid-king-targets   board idx is-white-piece?) ]
          [(= piece black-queen)  (valid-queen-targets  board idx is-white-piece?) ]
          [(= piece black-rook)   (valid-rook-targets   board idx is-white-piece?) ]
          [(= piece black-bishop) (valid-bishop-targets board idx is-white-piece?) ]
          [(= piece black-knight) (valid-knight-targets board idx is-white-piece?) ]
          [(= piece black-pawn)   (valid-pawn-targets   board idx is-white-piece?) ])))

; Return a list of valid moves e.g. '( (src-idx . dst-idx) (src-idx . dst-idx) ... )
(define (valid-moves board is-color?)
  ; Loop over indices of pieces
  (let index-loop ([result '()]
                   [indices (get-piece-indices board is-color?)])
    (if (null? indices)
        result
        ; Loop over valid targets of a particular index
        (let ([src-idx (car indices)])
          (index-loop
           (let moves-loop ([moves result]
                            [targets (valid-targets board src-idx)])
             (if (null? targets)
                 moves
                 (moves-loop (cons (cons src-idx (car targets)) moves)
                             (cdr targets))))
           (cdr indices))))))

(define (is-black-piece? piece)
  (and (>= piece black-pawn) (<= piece black-king)))

(define (is-white-piece? piece)
  (and (>= piece white-pawn) (<= piece white-king)))

(define (is-empty? piece)
  (= piece empty))

; Ignores en passant, pawn promotion & moving into check
(define (valid-pawn-targets board idx is-opposite-color?)
  (define-values (forward initial-rank)
    (if (eq? is-opposite-color? is-black-piece?)
        (values 12 2)
        (values -12 7)))
  (let* ([forward-empty? (is-empty? (bytes-ref board (+ idx forward)))]
         [result '()]
         ; Forward 1
         [result (if forward-empty?
                     (cons (+ idx forward) result)
                     result)]
         ; Forward 2
         [result (if (and (= initial-rank (idx->rank idx))
                      forward-empty?
                      (is-empty? (bytes-ref board (+ idx (* 2 forward)))))
                     (cons (+ idx (* 2 forward)) result)
                     result)]
         ; Capture forward higher index
         [result (if (is-opposite-color? (bytes-ref board (+ idx forward 1)))
                     (cons (+ idx forward 1) result)
                     result)]
         ; Capture forward lower index
         [result (if (is-opposite-color? (bytes-ref board (+ idx forward -1)))
                     (cons (+ idx forward -1) result)
                     result)])
    result))

(define (valid-knight-targets board idx is-opposite-color?)
  (valid-target-list board idx is-opposite-color? '(25 14 -10 -23 -25 -14 10 23)))

(define (valid-king-targets board idx is-opposite-color?)
  (valid-target-list board idx is-opposite-color?
                     (list north north-east east south-east south south-west west north-west)))

; Filter the list of possible targets to only the valid ones
(define (valid-target-list board idx is-opposite-color? targets)
  (~> (map (curry + idx) targets)
      (filter (λ (n)
                (let ([target (bytes-ref board n)])
                  (or (is-empty? target)
                      (is-opposite-color? target)))) _)))

; Return a list of valid target destinations in the specified direction
(define (valid-sliding-targets board idx is-opposite-color? direction)
  (let loop ([i (+ idx direction)] [accum '()])
    (let ([target (bytes-ref board i)])
      (cond [(is-empty? target) (loop (+ i direction) (cons i accum))]
            [(is-opposite-color? target) (cons i accum)]
            [else accum]))))

(define (valid-bishop-targets board idx is-opposite-color?)
  (append-map (curry valid-sliding-targets board idx is-opposite-color?)
              (list north-east south-east south-west north-west)))

(define (valid-rook-targets board idx is-opposite-color?)
  (append-map (curry valid-sliding-targets board idx is-opposite-color?)
              (list north east south west)))

; Apply each function given to the same set of arguments,
; then concatenate the result lists into a single list.
(define (juxt . funs)
  (λ args (append-map (λ (f) (apply f args))
                      funs)))

(define valid-queen-targets
  (juxt valid-bishop-targets
        valid-rook-targets))

(define (piece-symbol piece)
  (match piece
    [ (== white-king)   #\K ]
    [ (== white-queen)  #\Q ]
    [ (== white-rook)   #\R ]
    [ (== white-bishop) #\B ]
    [ (== white-knight) #\N ]
    [ (== white-pawn)   #\P ]
    [ (== black-king)   #\k ]
    [ (== black-queen)  #\q ]
    [ (== black-rook)   #\r ]
    [ (== black-bishop) #\b ]
    [ (== black-knight) #\n ]
    [ (== black-pawn)   #\p ]
    [ _                 #\_ ]))

(define (get-piece-indices board pred)
  (let loop ([idx 0] [indices '()])
    (if (< idx board-length)
        (loop (add1 idx)
              (if (pred (bytes-ref board idx))
                  (cons idx indices)
                  indices))
        indices)))

; For now, use a simplistic strategy of simply summing the value of
; all the pieces.
(define (evaluate-board board)
  (let loop ([idx 0] [score 0.0])
    (if (< idx board-length)
        (loop (add1 idx) (+ score (piece-value (bytes-ref board idx))))
        score)))

; Print a text representation of the board
(define (print-board board)
  (for ([rank '(#\8 #\7 #\6 #\5 #\4 #\3 #\2 #\1)])
       (for ([file '(#\a #\b #\c #\d #\e #\f #\g #\h)])
            (let* ([pos (bytes (char->integer file) (char->integer rank))]
                   [piece (bytes-ref board (pos->idx pos))])
              (display (piece-symbol piece))
              (display " ")))
       (displayln "")))

(define (flip-color-pred pred)
  (if (eq? pred is-white-piece?)
      is-black-piece?
      is-white-piece?))

(define (apply-move board move)
  (let* ([new-board (bytes-copy board)]
         [src-idx (car move)]
         [dst-idx (cdr move)]
         [src-piece (bytes-ref new-board src-idx)])
    (bytes-set! new-board src-idx empty)
    (bytes-set! new-board dst-idx src-piece)
    new-board))

; Minimax
; https://www.thanassis.space/score4.html
(define (minimax board is-color? level evaluator)
  (define maximizingPlayer (eq? is-color? is-white-piece?))
  (define comparator (if maximizingPlayer > <))

  (if (< level 1)
      (cons #f (evaluator board))
      (let loop ([moves (valid-moves board is-color?)]
                 [best (cons #f
                             (if maximizingPlayer
                                 min-score
                                 max-score))])
        (if (null? moves)
            best
            (let* ([move (car moves)]
                   [move-score (minimax (apply-move board move)
                                        (flip-color-pred is-color?)
                                        (- level 1)
                                        evaluator)])
              (loop (cdr moves)
                    (if (comparator (cdr move-score)
                                    (cdr best))
                        (cons move (cdr move-score))
                        best)))))))

(define (display-move move)
  (display (idx->pos (car move)))
  (display " to ")
  (displayln (idx->pos (cdr move)))
  (displayln ""))

(define (game board depth)
  (let loop ([board board])
    (define move-score (minimax board is-white-piece? depth evaluate-board))
    (define move (car move-score))
    (display-move move)
    (define board2 (apply-move board move))
    (print-board board2)
    (displayln "")
    (display "Enter source")
    (define src (string->bytes/utf-8 (read-line)))
    (display "Enter destination")
    (define dst (string->bytes/utf-8 (read-line)))
    (define board3 (apply-move board2 (cons (pos->idx src) (pos->idx dst))))
    (print-board board3)
    (displayln "")
    (loop board3)))

(game board 5)

;(board-set! board #"e2" empty)
;(print-board board)
;(map idx->pos (valid-targets board (pos->idx #"e2")))
;(map idx->pos (get-piece-indices board is-white-piece?))

;(define board2 (apply-move board (cons (pos->idx #"b2") (pos->idx #"b3"))))

;(print-board board75)

;(define move-score (minimax board83 is-white-piece? 6 evaluate-board))
;move-score
;(cdr move-score)
;(cons (idx->pos (caar move-score))
;      (idx->pos (cdar move-score)))
