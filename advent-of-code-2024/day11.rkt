#lang racket

(require "./advent.rkt" memo)

(define/memoize (blink stone blinks)
  (cond
    [ (zero? blinks)  1                       ]
    [ (zero? stone)   (blink 1 (sub1 blinks)) ]
    [ else
      (let*-values ([ (s)   (number->string stone)                   ]
                    [ (l r) (quotient/remainder (string-length s) 2) ])
        (if (zero? r)
            (+ (blink (string->number (substring s 0 l)) (sub1 blinks))
               (blink (string->number (substring s l)) (sub1 blinks)))
            (blink (* stone 2024) (sub1 blinks)))) ]))

(define (solve blinks)
  (for/sum ([ stone (in-list (car (parse-aoc 11 numbers))) ])
    (blink stone blinks)))

;; --------------------------------------------------------------------------------------------

(check-equal? (solve 25) 183484)
(check-equal? (solve 75) 218817038947400)
