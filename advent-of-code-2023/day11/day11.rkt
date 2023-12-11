#lang racket
(require "../advent.rkt")

(define-values (rows columns galaxies)
  (let ([ empty? (compose1 not false? (curry andmap (curry char=? #\.))) ]
        [ lines  (parse-aoc 11 string->list) ])
    (values (for/list ([ i (in-naturals) ][ line (in-list lines) ]
                       #:when (empty? line)) i)
            (for/list ([ i (in-range (length (car lines))) ]
                       #:when (empty? (map (curry (flip list-ref) i) lines))) i)
            (for/list ([ row (in-naturals) ][ line (in-list lines) ] #:when #t
                       [ col (in-naturals) ][ ch (in-list line) ] #:when (char=? #\# ch))
                      (make-rectangular col row)))))

(define (distance pos1 pos2 inc)
  (define (expanded a b lst)
    (count (λ (n)
             (and (> n (min a b))
                  (< n (max a b))))
           lst))

  (let* ([ x1 (real-part pos1) ]
         [ y1 (imag-part pos1) ]
         [ x2 (real-part pos2) ]
         [ y2 (imag-part pos2) ])
    (+ (abs (- x1 x2))
       (abs (- y1 y2))
       (* (expanded y1 y2 rows)   (sub1 inc))
       (* (expanded x1 x2 columns) (sub1 inc)))))

(define (solve mult)
  (~> galaxies
      (combinations _ 2)
      (map (match-lambda [ (list gal1 gal2)
                           (distance gal1 gal2 mult) ]) _)
      list-sum))

;; Tests --------------------------------------------------------------------------------------

(check-equal? (solve 2) 9734203)
(check-equal? (solve 1000000) 568914596391)
