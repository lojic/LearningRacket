#lang racket

(require "../advent.rkt")

(define cards (map (λ (l)
                     (set-count (set-intersect (drop (string-split (car l)) 2)
                                               (string-split (cadr l)))))
                   (parse-aoc 4 (λ (s) (string-split s " | ")))))

(define (part1 cards)
  (list-sum (map (λ (n) (expt 2 (sub1 n))) (filter positive? cards))))

(define (part2 n cards)
  (+ n (let loop ([ n n ][ cards cards ][ total 0 ])
         (cond [ (= n 0) total ]
               [ else (let ([ sub-total (part2 (car cards) (cdr cards)) ])
                        (loop (sub1 n) (cdr cards) (+ total sub-total))) ]))))
