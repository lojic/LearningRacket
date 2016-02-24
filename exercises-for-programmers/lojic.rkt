#lang racket

(define (gets msg)
  (display msg)
  (display " ")
  (read-line))

(define (get-num msg)
  (let ([str (gets msg)])
    (if (regexp-match #px"^\\d+\\.?\\d*$" str)
        (string->number str)
        (begin
          (displayln "Invalid number")
          (get-num msg)))))

(define (get-boolean msg)
  (let ([str (gets msg)])
    (if (regexp-match #px"^(Yes|yes|Y|y|1|No|no|N|n|0)$" str)
        (regexp-match #px"^(Yes|yes|Y|y|1)$" str)
        (begin
          (displayln "Invalid boolean")
          (get-boolean msg)))))

(define (~0.2r x)
  (~r x #:precision (list '= 2)))

(define (~0.3r x)
  (~r x #:precision (list '= 3)))

(define (log10 x)
  (/ (log x) (log 10)))

(provide get-boolean get-num gets ~0.2r ~0.3r log10)
