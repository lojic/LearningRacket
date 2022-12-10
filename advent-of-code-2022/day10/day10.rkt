#lang racket
(require "../advent.rkt")

(define (cpu in step)
  (let loop ([ in     in  ]
             [ x      1   ]
             [ cycle  1   ]
             [ result '() ])
    (if (null? in)
        (reverse result)
        (loop (cdr in)                  ; Remainder of CPU instructions
              (+ x (car in))            ; Execute instruction
              (add1 cycle)              ; Increment cycle
              (step cycle x result))))) ; Delegate to Part step

(define (part1 cycle x result)
  (if (memq cycle '(20 60 100 140 180 220))
      (cons (* cycle x) result)
      result))

(define (part2 cycle x result)
  (cons (if (<= (sub1 x) (modulo (sub1 cycle) 40) (add1 x))
            #\#
            #\.)
        result))

(define (flatten-addx lst)
  (if (null? lst)
      '()
      (match (car lst)
        [ (list "addx" val) (cons 0 (cons val (flatten-addx (cdr lst)))) ]
        [ (list "noop")     (cons 0 (flatten-addx (cdr lst)))            ])))

(define (render lst)
  (for ([ line (chunk 40 lst) ])
    (printf "~a\n" (list->string line))))

(define in (flatten-addx (parse-aoc 10 atoms)))

(list-sum (cpu in part1)) ; Part 1
(render (cpu in part2))   ; Part 2
