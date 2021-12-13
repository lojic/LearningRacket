#lang racket

(define edges (make-hash))

(for ([ line (in-lines) ])
  (match-define (list a b) (string-split line "-"))
  (hash-set! edges a (cons b (hash-ref edges a '())))
  (hash-set! edges b (cons a (hash-ref edges b '()))))

(define (count node [ visited (set) ][ doubled #f ])
  (define (add-small) (if (string=? node (string-downcase node)) (set-add visited node) visited))

  (cond [ (string=? node "end") 1 ]
        [ else (for/sum ([ next (hash-ref edges node) ])
                 (cond [ (or (string=? next "start") (and (set-member? visited next) doubled)) 0 ]
                       [ else (count next (add-small) (if (set-member? visited next) #t doubled)) ])) ]))

(displayln (count "start"))

;; Port of https://github.com/hyper-neutrino/advent-of-code/blob/main/2021/day12p2.py
