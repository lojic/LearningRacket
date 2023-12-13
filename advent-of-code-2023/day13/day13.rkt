#lang racket
(require "../advent.rkt")

(define patterns  (map (curry map string->list) (parse-aoc 13 string-split #:sep "\n\n")))
(define transpose (curry apply map list))

(define (reflection? pos lst)
  (let-values ([ (left right) (split-at lst pos) ])
    (let ([ l-len  (length left)  ]
          [ r-len (length right) ])
      (cond [ (< l-len r-len) (equal? (reverse left) (take right l-len))           ]
            [ (< r-len l-len) (equal? (reverse (drop left (- l-len r-len))) right) ]
            [ else            (equal? (reverse left) right)                        ]))))

(define (vertical-reflection pat)
  (findf (λ (pos)
           (andmap (curry reflection? pos) pat))
         (inclusive-range 1 (sub1 (length (car pat))))))

(define (solve patterns)
  (for/sum ([ pat (in-list patterns) ])
    (+ (or (vertical-reflection pat) 0)
       (* 100 (or (vertical-reflection (transpose pat)) 0)))))

;; Tests --------------------------------------------------------------------------------------

(check-equal? (solve patterns) 30802)
