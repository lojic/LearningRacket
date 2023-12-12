#lang racket
(require "../advent.rkt")

(define records (map (match-lambda [ (list springs groups)
                                     (cons (string->list springs)
                                           (numbers groups)) ])
                     (parse-aoc 12 string-split)))

(define (invalid? springs groups)
  ;; placeholder
  #f)

(define (search input springs groups)
  (if (null? input)
      (list (reverse springs))
      (if (invalid? springs groups)
          (list '())
          (let ([ c (car input) ])
            (if (char=? #\? c)
                (append (search (cdr input) (cons #\. springs) groups)
                        (search (cdr input) (cons #\# springs) groups))
                (search (cdr input) (cons c springs) groups))))))

(define (part1)
  (for/sum ([ record (in-list records) ])
    (match-let ([ (cons springs groups) record ])
      (~> (search springs '() #f)
          (map (λ (l)
                 (~> (group-consecutive l)
                     (filter (λ (lst) (char=? #\# (car lst))) _)
                     (map length _))) _)
          (filter (λ (l)
                    (equal? l groups)) _)
          length))))

(part1)
