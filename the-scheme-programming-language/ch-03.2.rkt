#lang racket

(require racket/trace)

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(trace factorial)
(factorial 5)

(define (fact i a)
  (if (= i 0)
      a
      (fact (- i 1) (* a i))))
  
(define (factorial2 n)
  (fact n 1))

(trace fact)
(factorial2 5)
