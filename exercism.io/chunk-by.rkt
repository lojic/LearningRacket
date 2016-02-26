#lang racket

(provide chunk-by)

(define (chunk-by lst . fun)
  (define f (if (null? fun) identity (car fun)))
  (define (helper lst prev acc result)
    (if (null? lst)
        (reverse (cons (reverse acc) result))
        (let* ([e (car lst)]
               [val (f e)])
          (if (equal? val prev)
              (helper (cdr lst) val (cons e acc) result)
              (helper (cdr lst) val (list e) (cons (reverse acc) result))))))
  (helper (cdr lst) (f (car lst)) (list (car lst)) '()))

(module+ test
  (require rackunit rackunit/text-ui)

  (define suite
    (test-suite
     "chunk-by tests"
     
     (test-case "using default identify function with no chunks"
                (check-equal? (chunk-by '(1 2 3 4 5))
                              '((1) (2) (3) (4) (5))))
     (test-case "using default identify function"
                (check-equal? (chunk-by '(1 1 2 3 3 3 4 5 6 6 7 7))
                              '((1 1) (2) (3 3 3) (4) (5) (6 6) (7 7))))
     (test-case "using even? function"
                (check-equal? (chunk-by '(1 1 2 3 3 3 4 8 8 5 6 6 7 7) even?)
                              '((1 1) (2) (3 3 3) (4 8 8) (5) (6 6) (7 7))))
     ))
  
  (run-tests suite))
