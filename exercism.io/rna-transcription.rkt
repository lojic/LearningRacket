#lang racket
(require "listify.rkt")

(provide to-rna)

(define rna-map #hash((#\G . #\C) (#\C . #\G) (#\T . #\A) (#\A . #\U)))

(define (to-rna str)
  ((listify rna-transcriber) str))
   
(define (rna-transcriber list)
  (map (curry hash-ref rna-map) list))
