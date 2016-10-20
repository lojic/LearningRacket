#lang racket
(require racket/vector)
(require threading)

; TODO
; it may be easier to check for the case of moving a piece resulting in the king being checked per
; piece vs. analyzing the entire board after the fact. The reasoning being it should be a matter of
; checking a sliding "move" from the king to a sliding piece of the opposite color through the
; position being vacated.

; Initial board represented by a ByteString of 12x12=144 elements.
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

; File of idx
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

; Rank of idx
; (idx->rank 40) -> 2
(define (idx->rank idx)
  (- (quotient idx 12) 1))

; Convert a rank into an index where #\a is 0
; (rank->idx (char->integer #\b)) -> 1
(define (rank->idx rank)
  (- rank (char->integer #\a)))

; Convert a file number into an index #\1 is 0
; (file->idx (char->integer #2)) -> 1
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
(define board (let ([b (make-bytes 144 0)]
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

(define (piece-value piece)
  (match piece
    [ (== white-king)   99 ]
    [ (== white-queen)   9 ]
    [ (== white-rook)    5 ]
    [ (== white-bishop)  3 ]
    [ (== white-knight)  3 ]
    [ (== white-pawn)    1 ]
    [ (== black-king)   99 ]
    [ (== black-queen)   9 ]
    [ (== black-rook)    5 ]
    [ (== black-bishop)  3 ]
    [ (== black-knight)  3 ]
    [ (== black-pawn)    1 ]
    [ _                  0 ]))

(define (valid-moves board idx)
  (let ([piece (bytes-ref board idx)])
    (cond ([(= piece white-pawn) (valid-white-pawn-moves board idx)]))))

(define (is-black-piece? piece)
  (and (>= piece black-pawn) (<= piece black-king)))

(define (is-white-piece? piece)
  (and (>= piece white-pawn) (<= piece white-king)))

(define (is-empty? piece)
  (= piece empty))

; Ignores en passant, pawn promotion & moving into check
(define (valid-white-pawn-moves board idx)
  (let* ([forward-empty? (is-empty? (bytes-ref board (+ idx 12)))]
         [result '()]
         ; Forward 1
         [result (if forward-empty?
                     (cons (+ idx 12) result)
                     result)]
         ; Forward 2
         [result (if (and (= 2 (idx->rank idx))
                      forward-empty?
                      (is-empty? (bytes-ref board (+ idx 24))))
                     (cons (+ idx 24) result)
                     result)]
         ; Capture forward-right
         [result (if (is-black-piece? (bytes-ref board (+ idx 13)))
                     (cons (+ idx 13) result)
                     result)]
         ; Capture forward-left
         [result (if (is-black-piece? (bytes-ref board (+ idx 12)))
                     (cons (+ idx 12) result)
                     result)])
    result))

(define (valid-knight-moves board idx is-opposite-color?)
  (~> (map (curry + idx) '(25 14 -10 -23 -25 -14 10 23))
      (filter (λ (n)
                (let ([target (bytes-ref board n)])
                  (or (is-empty? target)
                      (is-opposite-color? target)))) _)))

(define (valid-sliding-moves board idx offset is-opposite-color?)
  (let loop ([i (+ idx offset)] [accum '()])
    (let ([target (bytes-ref board i)])
      (cond [(is-empty? target) (loop (+ i offset) (cons i accum))]
            [(is-opposite-color? target) (cons i accum)]
            [else accum]))))

(define (valid-bishop-moves board idx is-opposite-color?)
  '())

(define (valid-rook-moves board idx is-opposite-color?)
  '())

; For valid-queen-moves, simply combine valid-bishop and valid-rook
(define (valid-queen-moves board idx is-opposite-color?)
  '())

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
  

; Print a text representation of the board
(define (print-board board)
  (for ([rank '(#\8 #\7 #\6 #\5 #\4 #\3 #\2 #\1)])
       (for ([file '(#\a #\b #\c #\d #\e #\f #\g #\h)])
            (let* ([pos (bytes (char->integer file) (char->integer rank))]
                   [piece (bytes-ref board (pos->idx pos))])
              (display (piece-symbol piece))
              (display " ")))
       (displayln "")))
  
(print-board board)