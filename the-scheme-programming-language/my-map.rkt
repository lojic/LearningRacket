#lang racket

;; Define a version of map like the real one that can map a multi-argument procedure over multiple lists
(define (my-map f . ls)
  (if (null? (car ls))
      '()
      (cons (apply f (map car ls)) (apply my-map (cons f (map cdr ls))))))


