#lang racket

(require (for-syntax racket/syntax))
(provide state)

;; A macro to allow using:
;;
;; [ "forward" (state [ pos (+ (s-pos obj) n) ]) ]
;;
;; instead of:
;;
;; [ "forward" (struct-copy s obj [ pos (+ (s-pos obj) n) ]) ]
;;
;; within a match form.

(define-syntax (state stx)
  (syntax-case stx ()
    [(_ clause1 clause2 ...)
     (with-syntax ([ s (format-id stx "s") ]
                   [ obj (format-id stx "obj") ])
       #`(struct-copy s obj clause1 clause2 ...))]))

