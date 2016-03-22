#lang racket
(provide sum-square-diff)

(define (sum-square-diff n)
  (let ([nums (range 1 (+ n 1))]
        [square (λ (n) (* n n))])
    (- (square (apply + nums))
       (apply + (map square nums)))))
