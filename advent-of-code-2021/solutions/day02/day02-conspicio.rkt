#lang racket

;; This version uses ideas gleaned from other solutions

(require "syntax.rkt")
(struct s (aim x y))

(define (solve)
  (let ([ obj (foldl (λ (pair obj)
                       (let ([ command (car pair)                   ]
                             [ n       (string->number (cadr pair)) ])
                         (match command
                           [ "forward" (state [ x   (+ (s-x obj) n)                    ]
                                              [ y   (+ (s-y obj) (* (s-aim obj) n)) ]) ]
                           [ "down"    (state [ aim (+ (s-aim obj) n) ])               ]
                           [ "up"      (state [ aim (- (s-aim obj) n) ])               ])))
                     (s 0 0 0)
                     (map string-split (file->lines "day02.txt"))) ])
    (cons (* (s-x obj) (s-aim obj))
          (* (s-x obj) (s-y obj)))))

(solve)
