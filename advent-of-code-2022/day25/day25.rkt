#lang racket
(require "../advent.rkt")

(define BASE 5)

(define in (parse-aoc 25))

(define (digit-val c)
  (match c
    [ #\2  2 ]
    [ #\1  1 ]
    [ #\0  0 ]
    [ #\- -1 ]
    [ #\= -2 ]))

(define (snafu-val n)
  (vector-ref #(#\0 #\1 #\2 #\= #\-) (modulo n BASE)))

(define (snafu->decimal s)
  (let loop ([ chars (reverse (string->list s)) ]
             [ e     0                          ]
             [ sum   0                          ])
    (if (null? chars)
        sum
        (loop (cdr chars)
              (add1 e)
              (+ sum
                 (* (digit-val (car chars))
                    (expt BASE e)))))))

(define (decimal->snafu n)
  (let loop ([ n   n   ]
             [ lst '() ])
    (if (zero? n)
        (list->string lst)
        (let ([ n* (quotient (+ n 2) BASE) ])
          (loop n* (cons (snafu-val n) lst))))))

(define (part1)
  (decimal->snafu (list-sum (map snafu->decimal in))))

(check-equal? (part1) "2=-1=0")
