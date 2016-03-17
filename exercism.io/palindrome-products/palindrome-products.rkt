#lang racket
(provide largest-palindrome-product)

(define (palindromic? n)
  (define lst (string->list (number->string n)))
  (equal? lst (reverse lst)))

(define (largest-palindrome-product limit)
  (for*/fold ([result 0] [factors '()])
             ([a (range limit)]
              [b (range a limit)])
    (define p (* a b))
    (if (and (> p result) (palindromic? p))
        (values p (list a b))
        (values result factors))))
