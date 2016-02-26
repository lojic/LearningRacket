#lang racket
(require "../chunk-by.rkt")
(require threading)

(provide encode decode)

(define (encode string)
  (if (non-empty-string? string)
      (~> string
          string->list
          chunk-by
          (map (λ (x) (format "~a~a" (length x) (car x))) _)
          (string-join ""))
      ""))
      
(define (decode string)
  (cond [(non-empty-string? string)
         (define (pair-up lst)
           (if (null? lst)
               '()
               (cons (cons (string->number (list->string (car lst))) (cadr lst))
                     (pair-up (cddr lst)))))
         (~> string
             string->list
             (chunk-by char-numeric?)
             pair-up
             (foldr (λ (a b) (string-append (make-string (car a) (cadr a)) b)) "" _))]
        [else ""]))
