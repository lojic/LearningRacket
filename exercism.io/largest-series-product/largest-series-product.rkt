#lang racket
(provide largest-series-product)

(define (largest-series-product str n)
  (define digits (string->digits str))
  (define max-count (+ (- (length digits) n) 1))
  (let loop ([lst digits] [largest-product 0] [count 0])
    (if (< count max-count)
        (loop (cdr lst) (max largest-product (series-product lst n)) (+ count 1))
        largest-product)))

(define (string->digits str)
  (map (λ (c) (- (char->integer c) (char->integer #\0)))
       (string->list str)))

(define (series-product lst n)
  (apply * (take lst n)))