#lang racket

(require "./advent.rkt")

(define input (parse-aoc 7 numbers))

(define conc (λ (x y) (string->number (string-append (number->string x) (number->string y)))))

(define (is-valid? ops answer result operands)
  (cond [ (> result answer) #f                ]
        [ (null? operands)  (= answer result) ]
        [ else (ormap (λ (op)
                        (is-valid? ops answer (op result (car operands)) (cdr operands)))
                      ops) ]))

(define (solve . operators)
  (~> (filter (λ (lst)
                (is-valid? operators (car lst) (cadr lst) (cddr lst)))
              input)
      (map car _)
      (apply + _)))

(check-equal? (solve * +) 2664460013123)
(check-equal? (solve * + conc) 426214131924213)
