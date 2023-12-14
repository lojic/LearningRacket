#lang racket
(require "../advent.rkt")

;; Port of https://github.com/hyper-neutrino/advent-of-code/blob/main/2023/day13p2.py

(define (find-mirror grid)
  (or (for/or ([ r (in-inclusive-range 1 (length grid)) ])
        (let-values ([ (above below) (split-at grid r) ])
          (and (= 1 (for/sum ([ pair (zipn (reverse above) below) ])
                      (for/sum ([ pair (zipn (car pair) (cadr pair)) ])
                        (if (char=? (car pair) (cadr pair)) 0 1))))
               r)))
      0))

(for/sum ([ grid (map (curry map string->list) (parse-aoc 13 string-split #:sep "\n\n")) ])
  (+ (* (find-mirror grid) 100)
     (find-mirror (apply map list grid))))
