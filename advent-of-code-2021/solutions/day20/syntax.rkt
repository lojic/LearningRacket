#lang racket

(require (for-syntax racket/syntax))

(provide bit)

(define-syntax (bit stx)
  (syntax-case stx ()
    [(_ dx dy)
     (with-syntax ([ img       (format-id stx "img")       ]
                   [ x         (format-id stx "x")         ]
                   [ y         (format-id stx "y")         ]
                   [ get-pixel (format-id stx "get-pixel") ])
       #`(get-pixel img (+ x dx) (+ y dy))) ]))

