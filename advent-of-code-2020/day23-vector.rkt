#lang racket

;; Port of my Julia port of Jonathan Chan's code :)

(define (getcups input n)
  (let ([ cups (make-vector (add1 n)) ]
        [ len  (vector-length input)  ])
    (for ([ i (in-range 1 len) ])
      (vector-set! cups (vector-ref input (sub1 i)) (vector-ref input i)))

    (vector-set! cups (vector-ref input (sub1 (vector-length input))) (add1 len))

    (for ([ i (in-range (add1 len) n) ])
      (vector-set! cups i (add1 i)))

    (vector-set! cups n (vector-ref input 0))
    cups))

(define (prev idx) (if (= idx 1) 1000000 (sub1 idx)))

(define (play cups current)
  (let* ([ one   (vector-ref cups current) ]
         [ two   (vector-ref cups one)     ]
         [ three (vector-ref cups two)     ]
         [ four  (vector-ref cups three)   ]
         [ dest (let loop ([ dest (prev current) ])
                  (if (or (= dest one) (= dest two) (= dest three))
                      (loop (prev dest))
                      dest)) ]
         [ next (vector-ref cups dest) ])
    (vector-set! cups current four)
    (vector-set! cups three next)
    (vector-set! cups dest one)
    (vector-ref cups current)))

(define (part2 input)
  (let ([ cups (getcups input 1000000) ])
    (let loop ([ current (vector-ref input 0) ][ moves 0 ])
      (if (= moves 10000000)
          (let* ([ one (vector-ref cups 1)   ]
                 [ two (vector-ref cups one) ])
            (* one two))
          (loop (play cups current) (add1 moves))))))

(time (part2 #(4 9 6 1 3 8 5 2 7)))
