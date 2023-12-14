#lang racket
(require "../advent.rkt")

(define patterns  (map (curry map string->list) (parse-aoc 13 string-split #:sep "\n\n")))
(define transpose (curry apply map list))

(define (get-reflections pat)
  (define (reflection? pos lst)
    (let-values ([ (left right) (split-at lst pos) ])
      (let ([ l-len (length left) ]
            [ r-len (length right) ])
        (cond [ (< l-len r-len)  (equal? (reverse left) (take right l-len))           ]
              [ (< r-len l-len)  (equal? (reverse (drop left (- l-len r-len))) right) ]
              [ else             (equal? (reverse left) right)                        ]))))

  (filter (λ (pos)
            (andmap (curry reflection? pos) pat))
          (inclusive-range 1 (sub1 (length (car pat))))))

(define (solve part patterns)
  (for/sum ([ pat (in-list patterns) ])
    (let-values ([ (ver hor) (part pat) ])
      (+ (or ver 0) (* 100 (or hor 0))))))

(define (part1 pat)
  (let ([ ver (get-reflections pat)             ]
        [ hor (get-reflections (transpose pat)) ])
    (values (and (not (null? ver)) (car ver))
            (and (not (null? hor)) (car hor)))))

(define (part2 pat)
  (define (reflections pat)
    (append (map (λ (pos) (cons 'v pos)) (get-reflections pat))
            (map (λ (pos) (cons 'h pos)) (get-reflections (transpose pat)))))

  (define (fix-smudge pat row col)
    (define (update-col c)   (if (char=? #\. c) #\# #\.))
    (define (update-row lst) (list-update lst col update-col))
    (list-update pat row update-row))

  (match-let ([ (cons type pos)
                (for*/or ([ row (in-range (length pat))       ]
                          [ col (in-range (length (car pat))) ])
                  (let ([ reflections (set-subtract (reflections (fix-smudge pat row col))
                                                    (reflections pat)) ])
                    (and (not (null? reflections)) (car reflections))))])

    (values (and (equal? 'v type) pos)
            (and (equal? 'h type) pos))))

;; Tests --------------------------------------------------------------------------------------

(check-equal? (solve part1 patterns) 30802)
(check-equal? (solve part2 patterns) 37876)
