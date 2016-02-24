#lang racket

(define (reply s)
  (cond
    [(symbol=? s 'foo) "howdy"]
    [(symbol=? s 'bar) "there"]
    [(symbol=? s 'baz) "pilgrim"]
    [else "."]))
