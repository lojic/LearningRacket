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

(define (distance inc pair)
  (define (dist empties a b)
    (+ (abs (- a b))
       (* (sub1 inc)
          (count (λ (n) (< (min a b) n (max a b))) empties))))

  (+ (apply dist (cons rows    (map imag-part pair)))
     (apply dist (cons columns (map real-part pair)))))

(define (solve mult)
  (~> galaxies
      (combinations _ 2)
      (map (curry distance mult) _)
      list-sum))

;; Tests --------------------------------------------------------------------------------------

(check-equal? (solve 2) 9734203)
(check-equal? (solve 1000000) 568914596391)
