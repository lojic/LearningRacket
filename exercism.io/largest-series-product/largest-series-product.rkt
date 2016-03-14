#lang racket
(provide largest-series-product)

(define (largest-series-product str n)
  (let* ([digits (for/list ([c (in-string str)])
                           (- (char->integer c) (char->integer #\0)))]
         [max-count (+ (- (length digits) n) 1)]
         [series-product (λ (lst n) (apply * (take lst n)))])
    (let loop ([lst digits] [largest-product 0] [count 0])
      (if (< count max-count)
          (loop (cdr lst) (max largest-product (series-product lst n)) (+ count 1))
          largest-product))))
  