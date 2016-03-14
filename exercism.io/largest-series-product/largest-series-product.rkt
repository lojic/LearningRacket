#lang racket
(provide largest-series-product)

(define (largest-series-product str n)
  (let* ([digits (for/list ([c (in-string str)])
                           (- (char->integer c) (char->integer #\0)))]
         [max-count (+ (- (length digits) n) 1)])
    (let loop ([lst digits] [largest-product 0] [count 0])
      (if (< count max-count)
          (loop (cdr lst)
                (max largest-product
                     (series-product lst n 1))
                (+ count 1))
          largest-product))))

(define (series-product lst n acc)
  (if (> n 0)
      (series-product (cdr lst) (- n 1) (* (car lst) acc))
      acc))
