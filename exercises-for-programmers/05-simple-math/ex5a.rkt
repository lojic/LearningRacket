#lang racket
(require "../lojic.rkt")

(define num1 (string->number (gets "What is the first number? ")))
(define num2 (string->number (gets "What is the second number? ")))
(define (equation op)
  (format "~a ~a ~a = ~a" num1 (object-name op) num2 (op num1 num2)))
(display (string-join (map equation (list + - * /)) "\n"))
