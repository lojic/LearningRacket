#lang racket
(require "../advent.rkt")

(define in (~> (parse-aoc 10 atoms)
               (map (λ (inst)
                      (if (string=? "addx" (car inst))
                          (list 0 (cadr inst))
                          (list 0))) _)
               flatten
               (scanl + 1 _)))

(define (render lst)
  (for ([ line (chunk 40 lst) ])
    (printf "~a\n" (list->string line))))

(for/sum ([ cycle (in-inclusive-range 20 220 40) ]) ; Part 1
  (* cycle
     (list-ref in (- cycle 2))))

(render                                             ; Part 2
 (for/list ([ cycle (in-inclusive-range 1 240) ])
   (if (<= (abs (- (list-ref in (sub1 cycle))
                   (modulo cycle 40)))
           1)
       #\#
       #\.)))
