#lang racket
(require threading)
(provide abbreviate)

(define (abbreviate string)
  (~> string
      (string-split #px" |(?=[A-Z])")
      (filter non-empty-string? _)
      (map (λ (s) (substring s 0 1)) _)
      (string-join "")
      (string-upcase)))

