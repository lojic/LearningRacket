#lang racket
(provide chunk)

(define (get-chunk lst n)
  (let loop ([lst lst] [acc '()] [n n])
    (if (or (null? lst) (< n 1))
        (values (reverse acc) lst)
        (loop (cdr lst) (cons (car lst) acc) (- n 1)))))

(define (chunk lst n)
  (let loop ([lst lst] [acc '()])
    (if (null? lst)
        (reverse acc)
        (let-values ([(chunk rest) (get-chunk lst n)])
          (loop rest (cons chunk acc))))))
          