#lang racket
(require "../advent.rkt")

(define-values (rows columns galaxies)
  (let* ([ lines   (parse-aoc 11 string->list)                             ]
         [ empty?  (compose1 not false? (curry andmap (curry char=? #\.))) ]
         [ empties (λ (lines)
                     (~> (grid->hash lines #:row-filter empty? #:row-transform (const '(#t)))
                         (hash-keys _)
                         (map imag-part _))) ])

    (values (empties lines)

            (empties (apply map list lines)) ; Transpose lines to columns to reuse empties!

            (hash-keys (grid->hash lines #:col-filter (curry char=? #\#))))))

(define (distance inc pair)
  (define (dist empties a b)
    (+ (abs (- a b))
       (* (sub1 inc)
          (count (λ (n)
                   (< (min a b) n (max a b)))
                 empties))))

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
