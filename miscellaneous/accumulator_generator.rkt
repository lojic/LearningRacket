#lang racket/base

;; Paul Graham's "Accumulator Generator"
;; http://www.paulgraham.com/accgen.html

;; Scheme version:
;; (define (foo n)
;;   (λ (i) (set! n (+ n i)) n))

;; Ruby version:
;; def foo n
;;   ->(i) { n += i }
;; end

; Macro to get something similar to Ruby's += operator (i.e. Arc's ++ function)
(define-syntax ++
  (syntax-rules ()
    [(_ n i) (begin
               (set! n (+ n i))
               n)]))

(define (foo n)
  (λ (i) (++ n i)))
