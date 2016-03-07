#lang racket
(require "../chunk.rkt")
(require threading)
(provide encode)

(define mapping (let ([chars (string->list "abcdefghijklmnopqrstuvwxyz0123456789")])
                  (make-immutable-hash (map (λ (a b) (cons a b)) chars (reverse chars)))))

(define (encode plaintext)
  (~> (string-downcase plaintext)
      (string-replace #px"\\W" "")
      string->list
      (map (λ (c) (hash-ref mapping c)) _)
      (chunk 5)
      (map list->string _)
      (string-join " ")))
