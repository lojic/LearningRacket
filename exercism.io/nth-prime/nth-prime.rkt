#lang racket
(provide nth-prime)

(define (is-prime? n primes)
  (not (ormap (λ (p) (= 0 (remainder n p))) (reverse primes))))

(define (nth-prime n)
  (let loop ([cnt 1] [i 3] [primes '(2)])
    (cond [(= cnt n) (car primes)]
          [(is-prime? i primes) (loop (+ cnt 1) (+ i 2) (cons i primes))]
          [else (loop cnt (+ i 2) primes)])))
