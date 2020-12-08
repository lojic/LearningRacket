#lang racket

;; Inspired by a Python solution I saw (Part 2).

(define (run prog)
  (let loop ([ pc 0 ][ acc 0 ][ seen (mutable-set) ])
    (cond [ (set-member? seen pc) #f ]
          [ (>= pc (vector-length prog)) acc ]
          [ else
            (match-let ([ (vector op arg) (vector-ref prog pc)])
              (set-add! seen pc)
              (cond [ (string=? "acc" op) (loop (add1 pc) (+ acc arg) seen) ]
                    [ (string=? "jmp" op) (loop (+ pc arg) acc seen)        ]
                    [ else                (loop (add1 pc) acc seen)         ]))])))

(module+ main
  (define prog (for/vector ([ line (in-list (file->lines "day08.txt")) ])
                 (let ([ tokens (string-split line) ])
                   (vector (first tokens) (string->number (second tokens))))))

  (let loop ([ i 0 ][ p (vector-copy prog) ])
    (match-let* ([ (vector op arg) (vector-ref p i) ])
      (if (string=? "acc" op)
          (loop (add1 i) p)
          (let ([ new-op (if (string=? "nop" op) "jmp" "nop") ])
            (vector-set! p i (vector new-op arg))
            (let ([ result (run p) ])
              (if result
                  (printf "Accumulator is ~a\n" result)
                  (loop (add1 i) (vector-copy prog)))))))))
