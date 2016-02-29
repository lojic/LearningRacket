#lang racket
(provide primes-to)

(define (primes-to limit)
  (define v (make-vector (+ 1 limit) 'prime))
  (let loop ([i 2])
    (cond [(> i limit) (select-primes v)]
          [else (mark-multiples (+ i i) i v)
                (loop (next-prime (+ 1 i) v))])))

(define (mark-multiples mult n vec)
  (cond [(>= mult (vector-length vec)) '()]
        [else (vector-set! vec mult 'nonprime)
              (mark-multiples (+ mult n) n vec)]))

(define (next-prime i vec)
  (cond [(>= i (vector-length vec)) i]
        [(eq? 'prime (vector-ref vec i)) i]
        [else (next-prime (+ 1 i) vec)]))

(define (select-primes vec)
  (let loop ([i 2] [acc '()])
    (cond [(>= i (vector-length vec)) acc]
          [else (loop (+ 1 i) (if (eq? 'prime (vector-ref vec i))
                                  (cons i acc)
                                  acc))])))
