#lang racket

(provide hamming-distance)

(define (hamming-distance a b)
  (if (= (string-length a) (string-length b))
      (foldl (λ (x y sum) (+ sum (if (char=? x y) 0 1)))
             0
             (string->list a)
             (string->list b))
      '()))
      
