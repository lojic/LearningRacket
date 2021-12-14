#lang racket
(require graph threading)

(define g (~>> (sequence->list (in-lines))
               (map (curryr string-split "-"))
               undirected-graph))

(define (count node [ visited (set) ][ doubled #f ])
  (define (add-small) (if (string=? node (string-downcase node)) (set-add visited node) visited))

  (cond [ (string=? node "end") 1 ]
        [ else (for/sum ([ next (get-neighbors g node) ])
                 (cond [ (string=? next "start")                  0 ]
                       [ (and (set-member? visited next) doubled) 0 ]
                       [ else (count next (add-small) (if (set-member? visited next) #t doubled)) ])) ]))

(displayln (count "start"))

;; Port of https://github.com/hyper-neutrino/advent-of-code/blob/main/2021/day12p2.py
;; However, I made it completely functional, and simplified some parts.
