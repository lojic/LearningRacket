#lang racket

(require (prefix-in q: "./circular-queue.rkt"))
(require threading)

(define (cross-ref obj n)
  (define vec (make-vector (add1 n)))
  (let loop ([ n n ][ obj obj ])
    (cond [ (< n 1) vec ]
          [ else (vector-set! vec (q:node-val obj) obj)
                 (loop (sub1 n) (q:node-next obj)) ])))

(define (game obj num-moves max-n)
  (define vec (cross-ref obj max-n))
  (let loop ([ obj obj ][ moves 0 ])
    (cond [ (>= moves num-moves) (values obj vec) ]
          [ else (loop (move! obj max-n vec) (add1 moves)) ])))

(define (get-destination obj three max-n vec)
  (define v1 (q:node-val three))
  (define v2 (q:node-val (q:node-next three)))
  (define v3 (q:node-val (q:node-next (q:node-next three))))
  (define (in-removed? label) (or (eqv? label v1) (eqv? label v2) (eqv? label v3)))
  (define high (let loop ([ n max-n ]) (if (in-removed? n) (loop (sub1 n)) n)))
  (define low  (let loop ([ n 1 ]) (if (in-removed? n) (loop (add1 n)) n)))
  (let loop ([ label (sub1 (q:node-val obj)) ])
    (cond [ (< label low)             (loop high)            ]
          [ (not (in-removed? label)) (vector-ref vec label) ]
          [ else                      (loop (sub1 label))    ])))

(define (move! obj max-n vec)
  (let* ([ three (q:remove-n! (q:node-next obj) 3)     ]
         [ dest  (get-destination obj three max-n vec) ])
    (q:insert! dest three)
    (q:node-next obj)))

(define (part1 input num-moves)
  (define (answer obj vec)
    (~> (vector-ref vec 1)
        (q:queue->list _)
        (cdr _)
        (map number->string _)
        (string-join _ "")))
  (call-with-values (thunk (game (q:make-queue input) num-moves 9)) answer))

(define (part2 input num-nodes num-moves)
  (define (answer obj vec)
    (let* ([ fst (q:node-next (vector-ref vec 1)) ]
           [ snd (q:node-next fst)                ])
      (* (q:node-val fst) (q:node-val snd))))
  (define lst (let loop ([n (apply max input) ][ result (reverse input) ])
                (cond [ (>= n num-nodes) (reverse result) ]
                      [ else (let ([ n (add1 n) ]) (loop n (cons n result))) ])))
  (call-with-values (thunk (game (q:make-queue lst) num-moves num-nodes)) answer))

(module+ test (require rackunit)
  (check-equal? (part1 '(4 9 6 1 3 8 5 2 7) 100) "69425837")
  (check-equal? (time (part2 '(4 9 6 1 3 8 5 2 7) 1000000 10000000)) 218882971435))
