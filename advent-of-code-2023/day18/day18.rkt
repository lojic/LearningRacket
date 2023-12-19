#lang racket
(require "../advent.rkt")

;; Got 99% of this myself, but needed a hint (Pick's theorem) to finish it off.

(define input (parse-aoc 18 (λ (s)
                              (match-let ([ (list l n c) (string-split s) ])
                                (list (match l
                                        [ "R"  1 ]
                                        [ "L" -1 ]
                                        [ "U" -i ]
                                        [ "D" +i ]) (string->number n) c)))))

(define (part1 input)
  (define (create-edges origin input)
    (let loop ([ tail origin ][ input input ][ edges '() ])
      (if (null? input)
          (reverse edges)
          (match-let ([ (list dir n _) (car input) ])
            (let ([ head (+ tail (* dir n)) ])
              (loop head (cdr input) (cons (cons tail head) edges)))))))

  (define (reverse-input input)
    (map (λ (lst)
           (match-let ([ (list dir n c) lst ])
             (list (- dir) n c)))
         (reverse input)))

  (define (sum-border input)
    (for/fold ([ sum 0 ])
              ([ l (in-list input) ])
      (+ sum (cadr l))))

  (define (sum-edges edges)
    (for/fold ([ sum 0 ])
              ([ v (in-list edges) ])
      (match-let ([ (cons tail head) v ])
        (let ([ xt (real-part tail) ]
              [ yt (imag-part tail) ]
              [ xh (real-part head) ]
              [ yh (imag-part head) ])
          (+ sum (- (* xt yh) (* xh yt)))))))

  (let* ([ border (sum-border input) ]
         [ sum    (sum-edges (create-edges 0 input)) ]
         [ sum    (if (negative? sum)
                      sum
                      (sum-edges (create-edges 0 (reverse-input input)))) ])
    (+ 1 (/ border 2) (/ (- sum) 2))))

(define (part2)
  (part1 (for/list ([ lst (in-list input) ])
           (match-let ([ (list _ _ c) lst ])
             (list (match (substring c 7 8)
                     [ "0"  1 ]
                     [ "1" +i ]
                     [ "2" -1 ]
                     [ "3" -i ]) (string->number (substring c 2 7) 16) #f)))))

;; Tests --------------------------------------------------------------------------------------

(check-equal? (part1 input) 62365)
(check-equal? (part2) 159485361249806)
