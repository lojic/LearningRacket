#lang racket
(require "../advent.rkt")

;; A port of Ben Pollack's Nim code:
;; https://git.sr.ht/~bmp/aoc2022/tree/main/item/6/solver.nim

(define (solve len)
  (let ([ contents (file->string "./day06.txt") ])
    (let loop ([ i len ][ letters (mutable-set) ])
      (for ([ c (substring contents (- i len) i) ])
        (set-add! letters c))
      (if (= (set-count letters) len)
          i
          (loop (add1 i) (mutable-set))))))

(solve 4)
(solve 14)
