#lang racket

(provide add add-multiple grade)

(define (add db name grade)
  (hash-update db
               grade
               (λ (list) (sort (cons name list) string<?))
               '()))

(define (add-multiple db names-grades)
  (foldl (λ (pair result) (add result (car pair) (cadr pair)))
         db
         names-grades))

(define (grade db grade)
  (hash-ref db grade '()))