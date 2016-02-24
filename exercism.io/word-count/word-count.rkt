#lang racket
(require threading)
(provide word-count)

(define (word-count str)
  (~> str
      string-downcase
      (string-split #px"[^a-z]+")
      (foldl (λ (word hsh) (hash-update hsh word add1 0))
             (make-immutable-hash)
             _)))

