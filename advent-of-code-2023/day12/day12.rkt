#lang racket
(require "../advent.rkt")

;; Needed a hint to memoize, and since I already had to "cheat", I
;; also used some ideas from hyper-neutrino's solution.

(define records (map (match-lambda [ (list springs groups)
                                           (cons (string->list springs)
                                                 (numbers groups)) ])
                           (parse-aoc 12 string-split)))

(define cache (make-hash))

(define (search springs groups)
  (define (helper)
    (match (car springs)
      [ #\. (search (cdr springs) groups) ]
      [ #\# (let ([ group (car groups)     ]
                  [ len   (length springs) ])
              (cond [ (> group len)  0 ]
                    [ (memq #\. (take springs group))  0 ]
                    [ (= group len)  (if (null? (cdr groups)) 1 0) ]
                    [ (char=? #\# (list-ref springs group))  0 ]
                    [ else  (search (drop springs (add1 group)) (cdr groups)) ])) ]
      [ #\? (+ (search (cdr springs) groups)
               (search (cons #\# (cdr springs)) groups)) ]))

  (cond [ (null? springs) (if (null? groups) 1 0)     ]
        [ (null? groups)  (if (memq #\# springs) 0 1) ]
        [ else            (let* ([ key (list springs groups) ])
                            (or (hash-ref cache key #f)
                                (let ([ result (helper) ])
                                  (hash-set! cache key result)
                                  result))) ]))

(define (part2 records)
  (for/list ([ record (in-list records) ])
    (match-let ([ (cons springs groups) record ]
                [ q (list #\?) ])
      (cons (append springs q springs q springs q springs q springs)
            (append groups groups groups groups groups)))))

(define (solve records)
  (for/sum ([ rec (in-list records) ])
    (search (car rec) (cdr rec))))

;; Tests --------------------------------------------------------------------------------------

(check-equal? (solve records) 8419)
(check-equal? (solve (part2 records)) 160500973317706)
