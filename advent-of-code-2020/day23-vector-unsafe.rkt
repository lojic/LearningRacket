#lang racket

;; Port of my Julia port of Jonathan Chan's code :)
(require racket/fixnum)
(require racket/unsafe/ops)

(define (getcups input n)
  (let ([ cups (make-fxvector (add1 n)) ]
        [ len  (unsafe-fxvector-length input)  ])
    (for ([ i (in-range 1 len) ])
      (unsafe-fxvector-set! cups (unsafe-fxvector-ref input (unsafe-fx- i 1)) (unsafe-fxvector-ref input i)))

    (unsafe-fxvector-set! cups (unsafe-fxvector-ref input (unsafe-fx- (unsafe-fxvector-length input) 1)) (unsafe-fx+ len 1))

    (for ([ i (in-range (add1 len) n) ])
      (unsafe-fxvector-set! cups i (unsafe-fx+ i 1)))

    (unsafe-fxvector-set! cups n (unsafe-fxvector-ref input 0))
    cups))

(define (prev idx) (if (unsafe-fx= idx 1) 1000000 (unsafe-fx- idx 1)))

(define (play cups current)
  (let* ([ one   (unsafe-fxvector-ref cups current) ]
         [ two   (unsafe-fxvector-ref cups one)     ]
         [ three (unsafe-fxvector-ref cups two)     ]
         [ four  (unsafe-fxvector-ref cups three)   ]
         [ dest (let loop ([ dest (prev current) ])
                  (if (or (unsafe-fx= dest one) (unsafe-fx= dest two) (unsafe-fx= dest three))
                      (loop (prev dest))
                      dest)) ]
         [ next (unsafe-fxvector-ref cups dest) ])
    (unsafe-fxvector-set! cups current four)
    (unsafe-fxvector-set! cups three next)
    (unsafe-fxvector-set! cups dest one)
    (unsafe-fxvector-ref cups current)))

(define (part2 input)
  (let ([ cups (getcups input 1000000) ])
    (let loop ([ current (unsafe-fxvector-ref input 0) ][ moves 0 ])
      (if (unsafe-fx= moves 10000000)
          (let* ([ one (unsafe-fxvector-ref cups 1)   ]
                 [ two (unsafe-fxvector-ref cups one) ])
            (* one two))
          (loop (play cups current) (unsafe-fx+ moves 1))))))

(time (part2 #(4 9 6 1 3 8 5 2 7)))
