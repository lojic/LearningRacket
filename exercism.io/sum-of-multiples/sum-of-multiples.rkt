#lang racket
(provide sum-of-multiples)

(define (sum-of-multiples limit [factors '(3 5)])
  (apply + (remove-duplicates (apply append (map (curry multiples limit) factors)))))

(define (multiples limit factor)
  (let loop ([multiple factor] [acc '()])
    (if (< multiple limit)
        (loop (+ multiple factor) (cons multiple acc))
        acc)))
