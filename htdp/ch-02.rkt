#lang racket

(require htdp/convert)

(define (area-of-disk r)
  (* 3.14 (* r r)))

(define (area-of-ring outer inner)
  (- (area-of-disk outer)
     (area-of-disk inner)))

(define (Fahrenheit->Celsius t)
  (* (/ 5.0 9.0)
     (- t 32.0)))

(define (convert3 a b c)
  (+ a (* b 10) (* c 100)))
