#lang racket

(define (contains-doll? lst)
  (cond [(null? lst) #f]
        [(symbol=? (car lst) 'doll) #t]
        [else (contains-doll? (cdr lst))]))

(define (foo x y)
  (+ x y))

(define (bar n)
  (* 7 n))

(define foo-bar (compose bar foo))

(define-struct ir (name price))

(define (sum-inv lst)
  (cond [(null? lst) 0]
        [else (+ (ir-price (car lst)) (sum-inv (cdr lst)))]))


